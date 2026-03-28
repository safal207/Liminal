import type { Metadata } from 'next';
import './globals.css';
import Sidebar from '@/components/Sidebar';

export const metadata: Metadata = {
  title: 'LIMINAL — BurnoutGuard',
  description: 'AI-защита от выгорания. Мониторинг, анализ, рекомендации.',
};

export default function RootLayout({ children }: { children: React.ReactNode }) {
  return (
    <html lang="ru">
      <body style={{ display: 'flex', minHeight: '100vh' }}>
        <Sidebar />
        <main style={{ flex: 1, overflowY: 'auto', minHeight: '100vh' }}>
          {children}
        </main>
      </body>
    </html>
  );
}
