import pathlib
import sys

# Ensure project root on path so packages import without editable install
LOG_VIEWER = pathlib.Path(__file__).resolve().parents[1]
if str(LOG_VIEWER) not in sys.path:
    sys.path.insert(0, str(LOG_VIEWER))
