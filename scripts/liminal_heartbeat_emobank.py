"""Utility script for training and validating TinyRecursiveModelTRMv6 on EmoBank.

This is a direct port of the Colab notebook shared in the issue.  The main
reason the original notebook failed on GPU runtimes was that tensors and
modules were instantiated on the CPU by default while the
``SentenceTransformer`` encoder returns CUDA tensors when a GPU is available.
That led to multiple device mismatch errors during both training and the
optional baseline evaluation.

The script below keeps the original experimentation structure intact while
adding explicit device management so everything consistently lives on the same
device (CPU or CUDA).  It can be executed end-to-end or imported so that its
functions can be reused in tests.
"""

from __future__ import annotations

import argparse
import json
import urllib.request
import warnings
from collections import deque
from dataclasses import dataclass
from pathlib import Path
from typing import Iterable, List, Sequence, Tuple

import numpy as np
import pandas as pd
import torch
import torch.nn as nn
import torch.nn.functional as F
from sentence_transformers import SentenceTransformer
from sklearn.linear_model import LinearRegression
from sklearn.metrics import mean_absolute_error, mean_squared_error, r2_score

warnings.filterwarnings("ignore")


EMOBANK_URL = (
    "https://raw.githubusercontent.com/JULIELab/EmoBank/master/corpus/emobank.csv"
)


DEVICE = torch.device("cuda" if torch.cuda.is_available() else "cpu")


# ─────────────────────────────────────────────────────────────
# Model definitions (unchanged from the notebook apart from type hints)
# ─────────────────────────────────────────────────────────────


class SelfAttentionTiny(nn.Module):
    def __init__(self, dim: int = 128) -> None:
        super().__init__()
        self.q_proj = nn.Linear(dim, dim)
        self.k_proj = nn.Linear(dim, dim)
        self.v_proj = nn.Linear(dim, dim)
        self.out_proj = nn.Linear(dim, dim)
        self.scale = dim**-0.5

    def forward(self, x: torch.Tensor) -> Tuple[torch.Tensor, torch.Tensor]:
        Q = self.q_proj(x)
        K = self.k_proj(x)
        V = self.v_proj(x)
        attn = F.softmax(torch.bmm(Q, K.transpose(1, 2)) * self.scale, dim=-1)
        out = torch.bmm(attn, V)
        out = self.out_proj(out)
        return out, attn


class PADRegressionHead(nn.Module):
    def __init__(self, dim: int = 128) -> None:
        super().__init__()
        self.fc = nn.Sequential(
            nn.Linear(dim, dim // 2),
            nn.GELU(),
            nn.Linear(dim // 2, 3),
            nn.Tanh(),
        )

    def forward(self, z: torch.Tensor) -> torch.Tensor:
        return self.fc(z)


class SoulKernel(nn.Module):
    """Improved in-memory version from the notebook."""

    def __init__(self, dim: int = 128, memory_size: int = 100) -> None:
        super().__init__()
        self.dim = dim
        self.memory_buffer: deque[torch.Tensor] = deque(maxlen=memory_size)

    def forward(
        self,
        x: torch.Tensor,
        y: torch.Tensor,
        z: torch.Tensor,
        rinse_state: torch.Tensor,
        confs: Sequence[float],
    ) -> Tuple[torch.Tensor, torch.Tensor]:
        hope = torch.clamp(torch.mean(rinse_state.detach()), -1, 1)
        faith = (sum(confs) / len(confs)) ** 0.5 if confs else 0.0
        z = z + faith * 0.05 * z

        if len(self.memory_buffer) > 0:
            recent_states = torch.stack(list(self.memory_buffer)[-3:])
            future = torch.mean(recent_states, dim=0)
        else:
            future = torch.zeros_like(z[0:1]).expand_as(z)

        bond_value = torch.tanh(
            torch.mean(x * z).detach() + torch.mean(y * rinse_state).detach()
        )
        z = z + 0.2 * bond_value * (future - z)

        rinse_state = (rinse_state + future * 0.3).tanh()
        self.memory_buffer.append(rinse_state[0].detach().clone())

        return z, rinse_state


class TinyRecursiveModelTRMv6(nn.Module):
    def __init__(self, dim: int = 128, inner: int = 4, affect_w: float = 0.3) -> None:
        super().__init__()
        self.dim = dim
        self.inner = inner
        self.affect_w = affect_w

        self.ln_latent = nn.LayerNorm(dim * 3)
        self.ln_answer = nn.LayerNorm(dim * 2)
        self.ln_z = nn.LayerNorm(dim)

        self.latent = nn.Sequential(
            nn.Linear(dim * 3, dim * 2),
            nn.GELU(),
            nn.Linear(dim * 2, dim),
        )

        self.answer = nn.Sequential(
            nn.Linear(dim * 2, dim * 2),
            nn.GELU(),
            nn.Linear(dim * 2, dim),
        )

        self.affect_proj = nn.Sequential(nn.Linear(3, dim), nn.Tanh())

        self.affect_gate = nn.Sequential(nn.Linear(dim * 2, 1), nn.Sigmoid())

        self.attn_core = SelfAttentionTiny(dim)
        self.pad_head = PADRegressionHead(dim)
        self.soul = SoulKernel(dim)

    def forward(
        self,
        x: torch.Tensor,
        y_init: torch.Tensor,
        affect_vec: torch.Tensor | None = None,
        K: int = 5,
    ) -> Tuple[torch.Tensor, List[float], torch.Tensor]:
        y = y_init.clone()
        z = torch.zeros_like(y)
        confs: List[float] = []
        history: List[torch.Tensor] = []

        for _ in range(K):
            for _ in range(self.inner):
                latent_input = self.ln_latent(torch.cat([x, y, z], dim=-1))
                z_delta = self.latent(latent_input)
                z = self.ln_z(z + 0.3 * z_delta)

            if affect_vec is not None:
                affect_latent = self.affect_proj(affect_vec)
                gate = self.affect_gate(torch.cat([z, affect_latent], dim=-1))
                z = z + self.affect_w * gate * affect_latent
                confs.append(gate.mean().item())
            else:
                confs.append(0.0)

            history.append(z.unsqueeze(1))
            if len(history) > 1:
                seq = torch.cat(history, dim=1)
                attn_out, _ = self.attn_core(seq)
                z = self.ln_z(z + attn_out[:, -1, :])

            rinse_state = torch.tanh(z + y)
            z, rinse_state = self.soul(x, y, z, rinse_state, confs)

            answer_input = self.ln_answer(torch.cat([y, z], dim=-1))
            y = y + 0.4 * self.answer(answer_input)

        pad = self.pad_head(z)
        return y, confs, pad


# ─────────────────────────────────────────────────────────────
# Data utilities
# ─────────────────────────────────────────────────────────────


def ensure_tensor(value: torch.Tensor | np.ndarray) -> torch.Tensor:
    if isinstance(value, torch.Tensor):
        return value.to(DEVICE)
    return torch.tensor(value, device=DEVICE)


def encode_texts(
    encoder: SentenceTransformer, texts: Sequence[str], batch_size: int = 32
) -> torch.Tensor:
    if not texts:
        raise ValueError("No texts provided for encoding")

    embeddings = encoder.encode(
        texts,
        convert_to_tensor=True,
        show_progress_bar=len(texts) > batch_size,
        batch_size=batch_size,
    )
    return ensure_tensor(embeddings)


@dataclass
class DatasetSplits:
    train_X: torch.Tensor
    test_X: torch.Tensor
    train_y: torch.Tensor
    test_y: torch.Tensor


def download_emobank(dataset_path: Path, overwrite: bool = False) -> Path:
    if dataset_path.exists() and not overwrite:
        return dataset_path

    dataset_path.parent.mkdir(parents=True, exist_ok=True)
    urllib.request.urlretrieve(EMOBANK_URL, dataset_path)
    return dataset_path


def load_emobank(dataset_path: Path) -> pd.DataFrame:
    df = pd.read_csv(dataset_path)
    if not {"text", "split", "V", "A", "D"}.issubset(df.columns):
        raise ValueError("EmoBank CSV missing required columns")
    return df


def prepare_dataset(
    df: pd.DataFrame, encoder: SentenceTransformer, projection: nn.Module
) -> DatasetSplits:
    train_df = df[df["split"] == "train"].copy()
    test_df = df[df["split"] == "test"].copy()

    train_embeddings = encode_texts(encoder, train_df["text"].tolist())
    test_embeddings = encode_texts(encoder, test_df["text"].tolist())

    train_X = projection(train_embeddings)
    test_X = projection(test_embeddings)

    train_y = torch.tensor(train_df[["V", "A", "D"]].values, dtype=torch.float32)
    test_y = torch.tensor(test_df[["V", "A", "D"]].values, dtype=torch.float32)

    return DatasetSplits(
        train_X=train_X.to(DEVICE),
        test_X=test_X.to(DEVICE),
        train_y=train_y.to(DEVICE),
        test_y=test_y.to(DEVICE),
    )


# ─────────────────────────────────────────────────────────────
# Training & evaluation routines
# ─────────────────────────────────────────────────────────────


@dataclass
class TrainingConfig:
    epochs: int = 40
    batch_size: int = 64
    lr: float = 1e-3
    weight_decay: float = 1e-5
    patience: int = 10
    K: int = 5


@dataclass
class TrainingArtifacts:
    model: TinyRecursiveModelTRMv6
    history: dict[str, List[float]]


def train_model(
    splits: DatasetSplits, config: TrainingConfig
) -> TrainingArtifacts:
    model = TinyRecursiveModelTRMv6(dim=128, inner=4, affect_w=0.3).to(DEVICE)

    optimizer = torch.optim.Adam(
        list(model.parameters()), lr=config.lr, weight_decay=config.weight_decay
    )
    scheduler = torch.optim.lr_scheduler.ReduceLROnPlateau(
        optimizer, mode="min", patience=3, factor=0.5
    )

    best_val_loss = float("inf")
    patience_counter = 0
    best_state = None

    train_losses: List[float] = []
    val_losses: List[float] = []

    for epoch in range(config.epochs):
        model.train()
        epoch_losses: List[float] = []

        indices = torch.randperm(len(splits.train_X), device=DEVICE)
        for i in range(0, len(indices), config.batch_size):
            batch_idx = indices[i : i + config.batch_size]
            batch_x = splits.train_X[batch_idx]
            batch_y = splits.train_y[batch_idx]

            optimizer.zero_grad()
            y_init = torch.zeros(len(batch_x), 128, device=DEVICE)
            _, _, pad_pred = model(batch_x, y_init, affect_vec=None, K=config.K)
            loss = F.mse_loss(pad_pred, batch_y)
            loss.backward()
            torch.nn.utils.clip_grad_norm_(model.parameters(), 1.0)
            optimizer.step()
            epoch_losses.append(loss.item())

        model.eval()
        with torch.no_grad():
            y_init = torch.zeros(len(splits.test_X), 128, device=DEVICE)
            _, _, val_pad_pred = model(
                splits.test_X, y_init, affect_vec=None, K=config.K
            )
            val_loss = F.mse_loss(val_pad_pred, splits.test_y).item()

        train_loss = float(np.mean(epoch_losses))
        train_losses.append(train_loss)
        val_losses.append(val_loss)
        scheduler.step(val_loss)

        if val_loss < best_val_loss:
            best_val_loss = val_loss
            patience_counter = 0
            best_state = model.state_dict()
        else:
            patience_counter += 1

        if patience_counter >= config.patience:
            break

    if best_state is not None:
        model.load_state_dict(best_state)

    history = {"train": train_losses, "val": val_losses}
    return TrainingArtifacts(model=model, history=history)


def evaluate_model(
    model: TinyRecursiveModelTRMv6,
    splits: DatasetSplits,
    K: int = 5,
) -> Tuple[np.ndarray, np.ndarray, List[float]]:
    model.eval()
    with torch.no_grad():
        y_init = torch.zeros(len(splits.test_X), 128, device=DEVICE)
        _, confs, pad_pred = model(splits.test_X, y_init, affect_vec=None, K=K)

    return (
        splits.test_y.detach().cpu().numpy(),
        pad_pred.detach().cpu().numpy(),
        confs,
    )


def compute_metrics(
    y_true: np.ndarray, y_pred: np.ndarray
) -> Tuple[dict, dict, dict]:
    mae = {
        "overall": mean_absolute_error(y_true, y_pred),
        "V": mean_absolute_error(y_true[:, 0], y_pred[:, 0]),
        "A": mean_absolute_error(y_true[:, 1], y_pred[:, 1]),
        "D": mean_absolute_error(y_true[:, 2], y_pred[:, 2]),
    }
    rmse = {
        "overall": float(np.sqrt(mean_squared_error(y_true, y_pred))),
    }
    r2 = {
        "overall": r2_score(y_true, y_pred),
        "V": r2_score(y_true[:, 0], y_pred[:, 0]),
        "A": r2_score(y_true[:, 1], y_pred[:, 1]),
        "D": r2_score(y_true[:, 2], y_pred[:, 2]),
    }
    return mae, rmse, r2


def run_baseline(
    train_X: torch.Tensor, train_y: torch.Tensor, test_X: torch.Tensor
) -> np.ndarray:
    baseline = LinearRegression()
    baseline.fit(train_X.detach().cpu().numpy(), train_y.detach().cpu().numpy())
    return baseline.predict(test_X.detach().cpu().numpy())


def predict_emotion(
    model: TinyRecursiveModelTRMv6,
    projection: nn.Module,
    encoder: SentenceTransformer,
    text: str,
    *,
    K: int,
) -> Tuple[float, float, float]:
    embedding = encoder.encode([text], convert_to_tensor=True)
    x = projection(ensure_tensor(embedding))
    model.eval()
    with torch.no_grad():
        _, _, pad = model(x, torch.zeros(1, 128, device=DEVICE), K=K)

    v, a, d = pad[0].detach().cpu().numpy().tolist()
    return float(v), float(a), float(d)


def format_metrics_report(
    mae: dict, rmse: dict, r2: dict, baseline_mae: float, baseline_r2: float
) -> str:
    report_lines = [
        "Metric                Overall      Valence      Arousal      Dominance",
        "--------------------------------------------------------------------",
        f"MAE                   {mae['overall']:.4f}      {mae['V']:.4f}      {mae['A']:.4f}      {mae['D']:.4f}",
        f"RMSE                  {rmse['overall']:.4f}      -             -             -",
        f"R^2                   {r2['overall']:.4f}      {r2['V']:.4f}      {r2['A']:.4f}      {r2['D']:.4f}",
        "",
        "Baseline Comparison:",
        f"  Linear Regression  | MAE: {baseline_mae:.4f} | R^2: {baseline_r2:.4f}",
        f"  LIMINAL Heartbeat  | MAE: {mae['overall']:.4f} | R^2: {r2['overall']:.4f}",
        f"  Improvement        | {(baseline_mae - mae['overall']) / baseline_mae * 100:.1f}%",
    ]
    return "\n".join(report_lines)


# ─────────────────────────────────────────────────────────────
# CLI Entrypoint
# ─────────────────────────────────────────────────────────────


DEFAULT_SAMPLES: Tuple[str, ...] = (
    "I am so excited about this new opportunity!",
    "Feeling overwhelmed and exhausted today...",
    "Just a peaceful quiet morning",
)


def main(args: argparse.Namespace) -> None:
    dataset_path = Path(args.dataset)
    if args.auto_download:
        download_emobank(dataset_path, overwrite=args.overwrite)
    if not dataset_path.exists():
        raise FileNotFoundError(dataset_path)

    df = load_emobank(dataset_path)

    projection = nn.Linear(384, 128).to(DEVICE)
    encoder = SentenceTransformer(
        "paraphrase-multilingual-MiniLM-L12-v2", device=str(DEVICE)
    )

    splits = prepare_dataset(df, encoder, projection)

    config = TrainingConfig(
        epochs=args.epochs,
        batch_size=args.batch_size,
        lr=args.lr,
        weight_decay=args.weight_decay,
        patience=args.patience,
        K=args.iterations,
    )

    artifacts = train_model(splits, config)
    y_true, y_pred, confs = evaluate_model(artifacts.model, splits, K=config.K)

    mae, rmse, r2 = compute_metrics(y_true, y_pred)
    baseline_pred = run_baseline(splits.train_X, splits.train_y, splits.test_X)

    baseline_mae = mean_absolute_error(y_true, baseline_pred)
    baseline_r2 = r2_score(y_true, baseline_pred)

    results = {
        "mae": mae,
        "rmse": rmse,
        "r2": r2,
        "avg_confidence": float(np.mean(confs)),
        "baseline": {
            "mae": float(baseline_mae),
            "r2": float(baseline_r2),
        },
    }

    if args.output_json:
        print(json.dumps(results, indent=2, ensure_ascii=False))
    else:
        print(format_metrics_report(mae, rmse, r2, baseline_mae, baseline_r2))
        print(f"Average Confidence: {results['avg_confidence']:.4f}")

    if args.samples:
        samples: Iterable[str]
        if args.samples == "default":
            samples = DEFAULT_SAMPLES
        else:
            samples = [s.strip() for s in Path(args.samples).read_text().splitlines() if s.strip()]

        print("\nSample predictions:")
        for text in samples:
            v, a, d = predict_emotion(
                artifacts.model, projection, encoder, text, K=config.K
            )
            print(f"  ""{text}"" -> V={v:.3f}, A={a:.3f}, D={d:.3f}")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Train TinyRecursiveModelTRMv6 on EmoBank with device-safe ops"
    )
    parser.add_argument("dataset", type=str, help="Path to the emobank.csv file")
    parser.add_argument(
        "--auto-download",
        action="store_true",
        help="Download EmoBank to the dataset path if it is missing",
    )
    parser.add_argument(
        "--overwrite",
        action="store_true",
        help="Overwrite the dataset file when --auto-download is used",
    )
    parser.add_argument("--epochs", type=int, default=40)
    parser.add_argument("--batch-size", type=int, default=64)
    parser.add_argument("--lr", type=float, default=1e-3)
    parser.add_argument("--weight-decay", type=float, default=1e-5)
    parser.add_argument("--patience", type=int, default=10)
    parser.add_argument("--iterations", type=int, default=5, help="Recursion depth K")
    parser.add_argument(
        "--output-json",
        action="store_true",
        help="Print metrics as JSON instead of a formatted table",
    )
    parser.add_argument(
        "--samples",
        type=str,
        default="",
        help=(
            "Either 'default' to run built-in sample texts or a path to a file "
            "with one text per line for inference"
        ),
    )
    main(parser.parse_args())
