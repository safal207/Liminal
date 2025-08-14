from backend.pythia_flow.pythia_flow import FlowCoordinator

fc = FlowCoordinator()

response = fc.generate_response(
    user_input="я боюсь, что не справлюсь", concept="страх", target="надежда"
)

print(response)

fc.close()
