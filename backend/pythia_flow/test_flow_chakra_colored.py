from backend.pythia_flow.pythia_flow import FlowCoordinator

if __name__ == "__main__":
    fc = FlowCoordinator()

    try:
        print("\n=== Тест: Путь от 'страх' к 'надежда' с цветом чакры ===")
        user_input = "Я чувствую страх и неуверенность"
        response = fc.generate_response(user_input, concept="страх", target="надежда")
        print("\n🔍 Результат:")
        print(response)

    finally:
        fc.close()
