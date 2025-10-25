from fastapi import FastAPI, Depends, HTTPException, status
from fastapi.middleware.cors import CORSMiddleware
from fastapi.security import HTTPBearer, HTTPAuthorizationCredentials
from pydantic import BaseModel
from typing import Any, Dict, List, Optional, Tuple
import uuid
import json
from datetime import datetime, date
import sqlite3
import os
from pathlib import Path
try:
    from transformers import pipeline
except ImportError:
    pipeline = None


# Ð˜Ð½Ð¸Ñ†Ð¸Ð°Ð»Ð¸Ð·Ð°Ñ†Ð¸Ñ FastAPI
app = FastAPI(
    title="Emotime API",
    description="ÐŸÑ€Ð¾ÑÑ‚Ð¾Ð¹ API Ð´Ð»Ñ Ð°Ð½Ð°Ð»Ð¸Ð·Ð° ÑÐ¼Ð¾Ñ†Ð¸Ð¹ Ð² Ñ‚ÐµÐºÑÑ‚Ðµ",
    version="1.0.0"
)

app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

# Ð‘ÐµÐ·Ð¾Ð¿Ð°ÑÐ½Ð¾ÑÑ‚ÑŒ
security = HTTPBearer()

# ÐœÐ¾Ð´ÐµÐ»Ð¸ Ð´Ð°Ð½Ð½Ñ‹Ñ…
class TextRequest(BaseModel):
    text: str
    language: str = "en"

class EmotionResponse(BaseModel):
    emotions: Dict[str, float]
    confidence: float
    processing_time: float
    timestamp: str
    translated_text: Optional[str] = None
    translation_note: Optional[str] = None
    analysis_language: str = "en"

class UserCreate(BaseModel):
    email: str
    plan: str = "starter"

class UserResponse(BaseModel):
    id: int
    email: str
    api_key: str
    plan: str
    created_at: str

# ÐŸÑ€Ð¾ÑÑ‚Ð°Ñ Ð±Ð°Ð·Ð° Ð´Ð°Ð½Ð½Ñ‹Ñ… SQLite

# Database settings
DEFAULT_DB_PATH = Path(__file__).with_name("emotime.db")
DB_PATH = os.getenv("EMOTIME_DB_PATH", str(DEFAULT_DB_PATH))


def get_connection():
    """Create SQLite connection honoring environment override."""
    if DB_PATH.startswith('file:'):
        return sqlite3.connect(DB_PATH, uri=True)
    db_path_obj = Path(DB_PATH)
    if db_path_obj.parent and not db_path_obj.parent.exists():
        db_path_obj.parent.mkdir(parents=True, exist_ok=True)
    return sqlite3.connect(DB_PATH)


TRANSLATION_MODELS: Dict[Tuple[str, str], str] = {
    ("ru", "en"): "Helsinki-NLP/opus-mt-ru-en",
    ("uk", "en"): "Helsinki-NLP/opus-mt-uk-en",
    ("es", "en"): "Helsinki-NLP/opus-mt-es-en",
}

_translation_cache: Dict[Tuple[str, str], Any] = {}


def translate_to_english(text: str, source_language: str) -> Tuple[str, Optional[str], str]:
    """Translate arbitrary text to English if a transformer pipeline is available."""
    src = (source_language or "en").lower()
    stripped = text.strip()
    if not stripped:
        return text, None, src or "en"
    if src == "en":
        return stripped, None, "en"

    if pipeline is None:
        return text, f"Translation skipped: install 'transformers' to enable {src}->en translation.", src

    model_name = TRANSLATION_MODELS.get((src, "en"))
    if not model_name:
        return text, f"Translation skipped: no model configured for {src}->en.", src

    translator = _translation_cache.get((src, "en"))
    if translator is None:
        try:
            translator = pipeline("translation", model=model_name)
        except Exception as exc:  # pragma: no cover - depends on optional deps
            return text, f"Translation skipped: {exc}", src
        _translation_cache[(src, "en")] = translator

    try:
        translated = translator(stripped)[0]["translation_text"]
    except Exception as exc:  # pragma: no cover - depends on optional deps
        return text, f"Translation failed: {exc}", src

    return translated, None, "en"


def init_db():
    conn = get_connection()
    cursor = conn.cursor()
    
    cursor.execute('''
        CREATE TABLE IF NOT EXISTS users (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            email TEXT UNIQUE NOT NULL,
            api_key TEXT UNIQUE NOT NULL,
            plan TEXT DEFAULT 'starter',
            created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
        )
    ''')
    
    cursor.execute('''
        CREATE TABLE IF NOT EXISTS usage (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            user_id INTEGER,
            requests_count INTEGER DEFAULT 0,
            date DATE DEFAULT CURRENT_DATE,
            FOREIGN KEY (user_id) REFERENCES users (id)
        )
    ''')
    
    conn.commit()
    conn.close()

# ÐŸÑ€Ð¾ÑÑ‚Ð¾Ð¹ Ð°Ð½Ð°Ð»Ð¸Ð·Ð°Ñ‚Ð¾Ñ€ ÑÐ¼Ð¾Ñ†Ð¸Ð¹
def analyze_text_emotions(text: str) -> Dict[str, float]:
    emotions = {
        "joy": 0.0,
        "sadness": 0.0,
        "anger": 0.0,
        "fear": 0.0,
        "surprise": 0.0,
        "disgust": 0.0,
        "neutral": 0.0
    }
    
    text_lower = text.lower()
    
    joy_words = ["happy", "joy", "excited", "great", "wonderful", "amazing"]
    sadness_words = ["sad", "depressed", "unhappy", "miserable", "grief"]
    anger_words = ["angry", "mad", "furious", "rage", "hate", "annoyed"]
    fear_words = ["afraid", "scared", "terrified", "anxious", "worried"]
    surprise_words = ["surprised", "shocked", "amazed", "astonished"]
    disgust_words = ["disgusted", "revolted", "sickened", "appalled"]
    
    for word in joy_words:
        if word in text_lower:
            emotions["joy"] += 0.3
    for word in sadness_words:
        if word in text_lower:
            emotions["sadness"] += 0.3
    for word in anger_words:
        if word in text_lower:
            emotions["anger"] += 0.3
    for word in fear_words:
        if word in text_lower:
            emotions["fear"] += 0.3
    for word in surprise_words:
        if word in text_lower:
            emotions["surprise"] += 0.3
    for word in disgust_words:
        if word in text_lower:
            emotions["disgust"] += 0.3
    
    total = sum(emotions.values())
    if total == 0:
        emotions["neutral"] = 1.0
    else:
        emotions["neutral"] = max(0, 1.0 - total)
        for emotion in emotions:
            if emotion != "neutral":
                emotions[emotion] = min(1.0, emotions[emotion] / total)
    
    return emotions

# Ð¤ÑƒÐ½ÐºÑ†Ð¸Ð¸ Ð´Ð»Ñ Ñ€Ð°Ð±Ð¾Ñ‚Ñ‹ Ñ Ð¿Ð¾Ð»ÑŒÐ·Ð¾Ð²Ð°Ñ‚ÐµÐ»ÑÐ¼Ð¸
def get_user_by_api_key(api_key: str) -> Optional[Dict]:
    conn = get_connection()
    cursor = conn.cursor()
    cursor.execute('SELECT * FROM users WHERE api_key = ?', (api_key,))
    user = cursor.fetchone()
    conn.close()
    
    if user:
        return {
            "id": user[0],
            "email": user[1],
            "api_key": user[2],
            "plan": user[3],
            "created_at": user[4]
        }
    return None

def check_usage_limits(user_id: int, plan: str) -> bool:
    limits = {
        "starter": 1000,
        "growth": 10000,
        "enterprise": 100000
    }
    
    conn = get_connection()
    cursor = conn.cursor()
    cursor.execute('''
        SELECT COALESCE(SUM(requests_count), 0) 
        FROM usage 
        WHERE user_id = ? AND date = CURRENT_DATE
    ''', (user_id,))
    today_usage = cursor.fetchone()[0]
    conn.close()
    
    return today_usage < limits.get(plan, 1000)

def update_usage(user_id: int):
    conn = get_connection()
    cursor = conn.cursor()
    
    cursor.execute('''
        SELECT id FROM usage 
        WHERE user_id = ? AND date = CURRENT_DATE
    ''', (user_id,))
    
    existing = cursor.fetchone()
    if existing:
        cursor.execute('''
            UPDATE usage 
            SET requests_count = requests_count + 1 
            WHERE id = ?
        ''', (existing[0],))
    else:
        cursor.execute('''
            INSERT INTO usage (user_id, requests_count, date) 
            VALUES (?, 1, CURRENT_DATE)
        ''', (user_id,))
    
    conn.commit()
    conn.close()

# Dependency Ð´Ð»Ñ Ð¿Ñ€Ð¾Ð²ÐµÑ€ÐºÐ¸ API ÐºÐ»ÑŽÑ‡Ð°
async def get_current_user(credentials: HTTPAuthorizationCredentials = Depends(security)):
    api_key = credentials.credentials
    user = get_user_by_api_key(api_key)
    if not user:
        raise HTTPException(
            status_code=status.HTTP_401_UNAUTHORIZED,
            detail="Invalid API key"
        )
    return user

# ÐžÑÐ½Ð¾Ð²Ð½Ð¾Ð¹ endpoint Ð´Ð»Ñ Ð°Ð½Ð°Ð»Ð¸Ð·Ð° ÑÐ¼Ð¾Ñ†Ð¸Ð¹
@app.post("/analyze", response_model=EmotionResponse)
async def analyze_emotion(
    request: TextRequest,
    current_user: Dict = Depends(get_current_user)
):
    if not check_usage_limits(current_user["id"], current_user["plan"]):
        raise HTTPException(
            status_code=status.HTTP_429_TOO_MANY_REQUESTS,
            detail="Daily limit exceeded. Upgrade your plan for more requests."
        )
    
    start_time = datetime.now()
    emotions = analyze_text_emotions(request.text)
    processing_time = (datetime.now() - start_time).total_seconds()
    
    update_usage(current_user["id"])
    
    return EmotionResponse(
        emotions=emotions,
        confidence=0.85,
        processing_time=processing_time,
        timestamp=datetime.now().isoformat()
    )

# Endpoint Ð´Ð»Ñ ÑÐ¾Ð·Ð´Ð°Ð½Ð¸Ñ Ð¿Ð¾Ð»ÑŒÐ·Ð¾Ð²Ð°Ñ‚ÐµÐ»Ñ
@app.post("/users", response_model=UserResponse)
async def create_user(user_data: UserCreate):
    api_key = str(uuid.uuid4())
    
    conn = get_connection()
    cursor = conn.cursor()
    
    try:
        cursor.execute('''
            INSERT INTO users (email, api_key, plan) 
            VALUES (?, ?, ?)
        ''', (user_data.email, api_key, user_data.plan))
        
        user_id = cursor.lastrowid
        conn.commit()
        
        return UserResponse(
            id=user_id,
            email=user_data.email,
            api_key=api_key,
            plan=user_data.plan,
            created_at=datetime.now().isoformat()
        )
    except sqlite3.IntegrityError:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="User with this email already exists"
        )
    finally:
        conn.close()

# Endpoint Ð´Ð»Ñ Ð¿Ð¾Ð»ÑƒÑ‡ÐµÐ½Ð¸Ñ Ð¸Ð½Ñ„Ð¾Ñ€Ð¼Ð°Ñ†Ð¸Ð¸ Ð¾ Ð¿Ð¾Ð»ÑŒÐ·Ð¾Ð²Ð°Ñ‚ÐµÐ»Ðµ
@app.get("/users/me", response_model=UserResponse)
async def get_current_user_info(current_user: Dict = Depends(get_current_user)):
    return UserResponse(
        id=current_user["id"],
        email=current_user["email"],
        api_key=current_user["api_key"],
        plan=current_user["plan"],
        created_at=current_user["created_at"]
    )

# Endpoint Ð´Ð»Ñ Ð¿Ð¾Ð»ÑƒÑ‡ÐµÐ½Ð¸Ñ ÑÑ‚Ð°Ñ‚Ð¸ÑÑ‚Ð¸ÐºÐ¸ Ð¸ÑÐ¿Ð¾Ð»ÑŒÐ·Ð¾Ð²Ð°Ð½Ð¸Ñ
@app.get("/usage")
async def get_usage_stats(current_user: Dict = Depends(get_current_user)):
    conn = get_connection()
    cursor = conn.cursor()
    
    cursor.execute('''
        SELECT COALESCE(SUM(requests_count), 0) 
        FROM usage 
        WHERE user_id = ? AND date = CURRENT_DATE
    ''', (current_user["id"],))
    today_usage = cursor.fetchone()[0]
    
    cursor.execute('''
        SELECT COALESCE(SUM(requests_count), 0) 
        FROM usage 
        WHERE user_id = ? AND date >= date('now', '-7 days')
    ''', (current_user["id"],))
    week_usage = cursor.fetchone()[0]
    
    conn.close()
    
    limits = {
        "starter": 1000,
        "growth": 10000,
        "enterprise": 100000
    }
    
    return {
        "today_usage": today_usage,
        "week_usage": week_usage,
        "daily_limit": limits.get(current_user["plan"], 1000),
        "plan": current_user["plan"]
    }

# Health check
@app.get("/health")
async def health_check():
    return {"status": "healthy", "timestamp": datetime.now().isoformat()}

# Ð˜Ð½Ð¸Ñ†Ð¸Ð°Ð»Ð¸Ð·Ð°Ñ†Ð¸Ñ Ð±Ð°Ð·Ñ‹ Ð´Ð°Ð½Ð½Ñ‹Ñ… Ð¿Ñ€Ð¸ Ð·Ð°Ð¿ÑƒÑÐºÐµ
@app.on_event("startup")
async def startup_event():
    init_db()
    print("Emotime API started successfully!")

if __name__ == "__main__":
    import uvicorn
    uvicorn.run(app, host="0.0.0.0", port=8000)
