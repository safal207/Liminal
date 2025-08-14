import json

import requests

API_URL = "http://localhost:8000"
TOKEN_URL = f"{API_URL}/token"


def get_token():
    print(f"--- Attempting to get token from {TOKEN_URL} ---")
    try:
        response = requests.post(
            TOKEN_URL, json={"username": "testuser", "password": "testpass"}
        )
        print(f"Status Code: {response.status_code}")

        if response.status_code == 200:
            token_data = response.json()
            print("Successfully received token:")
            print(json.dumps(token_data, indent=2))
            return token_data.get("access_token")
        else:
            print("Error receiving token:")
            try:
                print(response.json())
            except json.JSONDecodeError:
                print(response.text)
        return None
    except requests.exceptions.RequestException as e:
        print(f"An exception occurred: {e}")
        return None


if __name__ == "__main__":
    get_token()
