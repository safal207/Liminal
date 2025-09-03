from unittest.mock import patch
from backend.pythia_flow.pythia_flow import FlowCoordinator

def test_generate_response():
    """
    Tests the generate_response method with a mocked input.
    """
    fc = FlowCoordinator()

    # We mock the input() function to return a specific value
    with patch('builtins.input', return_value='some emotion'):
        response = fc.generate_response(
            user_input="я боюсь, что не справлюсь",
            concept="страх",
            target="надежда"
        )

    # We assert that the response is not empty, which is a basic check
    assert response is not None
    assert isinstance(response, str)

    fc.close()
