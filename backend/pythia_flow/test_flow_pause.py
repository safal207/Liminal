from backend.pythia_flow.pythia_flow import FlowCoordinator

fc = FlowCoordinator()
response = fc.generate_response("Я ни на что не горжусь", concept="стыд", target="гордость")
print(response)
fc.close()
