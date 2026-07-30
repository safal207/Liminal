# syntax=docker/dockerfile:1.7

# Supply an immutable reference such as:
# python:3.11-slim@sha256:<digest>
ARG PYTHON_IMAGE

FROM ${PYTHON_IMAGE} AS builder

ENV PIP_DISABLE_PIP_VERSION_CHECK=1 \
    PIP_NO_CACHE_DIR=1 \
    PYTHONDONTWRITEBYTECODE=1

WORKDIR /build

COPY requirements-core.txt ./requirements-core.txt

RUN python -m pip install --upgrade pip wheel setuptools && \
    python -m pip install --prefix=/install --prefer-binary \
      -r requirements-core.txt && \
    python -m pip check

FROM ${PYTHON_IMAGE} AS production

ENV PYTHONDONTWRITEBYTECODE=1 \
    PYTHONUNBUFFERED=1 \
    PYTHONPATH=/app

RUN apt-get update && \
    apt-get install -y --no-install-recommends curl && \
    rm -rf /var/lib/apt/lists/* && \
    useradd --create-home --shell /usr/sbin/nologin app

COPY --from=builder /install /usr/local

WORKDIR /app
COPY --chown=app:app backend ./backend

RUN mkdir -p /app/logs /app/data && chown -R app:app /app

USER app

HEALTHCHECK --interval=30s --timeout=10s --start-period=10s --retries=3 \
    CMD curl -fsS http://127.0.0.1:8000/ready || exit 1

EXPOSE 8000

CMD ["uvicorn", "backend.main:app", "--host", "0.0.0.0", "--port", "8000", "--workers", "4"]
