from backend.pythia_flow.pythia_flow import FlowCoordinator

fc = FlowCoordinator()

response = fc.generate_response(
    user_input="Я ни на что не горжусь", concept="стыд", target="гордость"
)

print("\n🔍 Результат:")
print(response)

fc.close()
