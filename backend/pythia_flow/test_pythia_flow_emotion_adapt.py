from unittest.mock import patch
from backend.pythia_flow.pythia_flow import FlowCoordinator

def test_generate_response_emotion_adapt():
    """
    Tests the generate_response method with emotion adaptation.
    """
    fc = FlowCoordinator()

    with patch('builtins.input', return_value='some other emotion'):
        response = fc.generate_response(
            user_input="Я ни на что не горжусь",
            concept="стыд",
            target="гордость"
        )

    assert response is not None
    assert isinstance(response, str)

    fc.close()
