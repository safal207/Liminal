
import sys
sys.path.append(".")
from web_interface import app, LiminalWebInterface

# Override port for testing
if __name__ == "__main__":
    app.run(debug=False, host="127.0.0.1", port=5001)
