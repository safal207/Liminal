// main.dart - Liminal Pulse Flutter App
import 'package:flutter/material.dart';
import 'package:http/http.dart' as http;
import 'dart:convert';

void main() => runApp(LiminalPulseApp());

class LiminalPulseApp extends StatelessWidget {
  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      title: 'Liminal Pulse',
      theme: ThemeData(
        colorScheme: ColorScheme.fromSeed(seedColor: Colors.deepPurple),
        useMaterial3: true,
      ),
      home: PulsePage(),
    );
  }
}

class PulsePage extends StatefulWidget {
  @override
  _PulsePageState createState() => _PulsePageState();
}

class _PulsePageState extends State<PulsePage> {
  String _status = "";
  bool _isLoading = false;

  Future<void> sendPulse() async {
    setState(() {
      _isLoading = true;
      _status = "Отправка...";
    });

    try {
      final pulse = 72; // Заглушка для пульса
      final response = await http.post(
        // Для веб-разработки используйте localhost
        Uri.parse('http://localhost:8000/pulse'),
        headers: {'Content-Type': 'application/json'},
        body: jsonEncode({
          'user_id': 'bro123',
          'pulse': pulse,
          'timestamp': DateTime.now().toIso8601String(),
        }),
      );

      if (response.statusCode == 200) {
        setState(() {
          _status = "✅ Пульс отправлен успешно!";
        });
        print("✅ Отправлено: ${response.body}");
      } else {
        setState(() {
          _status = "❌ Ошибка: ${response.statusCode}";
        });
      }
    } catch (e) {
      setState(() {
        _status = "❌ Ошибка подключения: $e";
      });
      print("❌ Ошибка: $e");
    } finally {
      setState(() {
        _isLoading = false;
      });
    }
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: Text("Liminal Pulse"),
        backgroundColor: Theme.of(context).colorScheme.inversePrimary,
      ),
      body: Center(
        child: Column(
          mainAxisAlignment: MainAxisAlignment.center,
          children: [
            Text(
              "💓 Resonance Liminal",
              style: Theme.of(context).textTheme.headlineMedium,
            ),
            SizedBox(height: 30),
            _isLoading
                ? CircularProgressIndicator()
                : ElevatedButton(
                    onPressed: sendPulse,
                    child: Text("📤 Отправить пульс"),
                  ),
            SizedBox(height: 20),
            Text(
              _status,
              style: TextStyle(
                fontSize: 16,
                color: _status.contains("✅") ? Colors.green : Colors.red,
              ),
            ),
          ],
        ),
      ),
    );
  }
}