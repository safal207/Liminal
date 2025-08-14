import asyncio
import json
import os
import time

import websockets


async def main():
    # Backend WebSocket URL and optional JWT token
    URL = os.getenv("WS_URL", "ws://127.0.0.1:8000/ws")
    TOKEN = os.getenv("WS_TOKEN", "")  # put JWT here if required
    uri = URL + (f"?token={TOKEN}" if TOKEN else "")

    # Burst parameters
    burst = int(os.getenv("BURST", "40"))  # how many messages to send quickly
    delay = float(os.getenv("DELAY", "0.01"))  # seconds between messages

    rate_limited_hints = 0
    received = 0
    start = time.time()

    # Disable client-side ping to avoid interference
    async with websockets.connect(uri, ping_interval=None) as ws:
        # Optional: subscribe to a channel if backend supports it
        try:
            await ws.send(json.dumps({"type": "subscribe", "channel": "test"}))
            try:
                await asyncio.wait_for(ws.recv(), timeout=1.0)
            except asyncio.TimeoutError:
                pass
        except Exception:
            pass

        # Quick burst of messages
        for i in range(burst):
            msg = {"type": "message", "channel": "test", "text": f"hi{i}"}
            await ws.send(json.dumps(msg))
            try:
                resp = await asyncio.wait_for(ws.recv(), timeout=0.05)
                received += 1
                text = str(resp).lower()
                if "429" in text or "rate" in text:
                    rate_limited_hints += 1
            except asyncio.TimeoutError:
                pass
            if delay:
                await asyncio.sleep(delay)

        duration = time.time() - start
        print(
            f"Sent: {burst}, received: {received}, rate-limited hints: {rate_limited_hints}, duration: {duration:.2f}s"
        )

        # Wait a moment for late responses
        try:
            for _ in range(10):
                resp = await asyncio.wait_for(ws.recv(), timeout=0.05)
                received += 1
                text = str(resp).lower()
                if "429" in text or "rate" in text:
                    rate_limited_hints += 1
        except Exception:
            pass

        print(f"Final received: {received}, rate-limited hints: {rate_limited_hints}")


if __name__ == "__main__":
    asyncio.run(main())
