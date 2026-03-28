'use client';

import Link from 'next/link';
import { usePathname } from 'next/navigation';

const NAV = [
  { href: '/',               icon: '🛡️', label: 'Dashboard'       },
  { href: '/analysis',       icon: '📊', label: 'Анализ риска'    },
  { href: '/recommendations',icon: '💡', label: 'Рекомендации'   },
  { href: '/progress',       icon: '📈', label: 'Прогресс'        },
];

export default function Sidebar() {
  const path = usePathname();

  return (
    <aside style={{
      width: 220,
      minHeight: '100vh',
      background: 'var(--surface)',
      borderRight: '1px solid var(--border)',
      display: 'flex',
      flexDirection: 'column',
      padding: '24px 0',
      flexShrink: 0,
    }}>
      {/* Logo */}
      <div style={{ padding: '0 20px 24px', borderBottom: '1px solid var(--border)' }}>
        <div style={{ fontSize: 20, fontWeight: 700, color: 'var(--primary)', letterSpacing: '-0.5px' }}>
          LIMINAL
        </div>
        <div style={{ fontSize: 11, color: 'var(--text2)', marginTop: 2 }}>BurnoutGuard</div>
      </div>

      {/* Nav */}
      <nav style={{ padding: '16px 10px', flex: 1 }}>
        {NAV.map(({ href, icon, label }) => {
          const active = href === '/' ? path === '/' : path.startsWith(href);
          return (
            <Link key={href} href={href} style={{
              display: 'flex',
              alignItems: 'center',
              gap: 10,
              padding: '9px 12px',
              borderRadius: 8,
              marginBottom: 4,
              background: active ? 'rgba(156,39,176,0.15)' : 'transparent',
              color: active ? 'var(--primary)' : 'var(--text2)',
              fontWeight: active ? 600 : 400,
              fontSize: 14,
              transition: 'all .15s',
            }}>
              <span style={{ fontSize: 16 }}>{icon}</span>
              {label}
            </Link>
          );
        })}
      </nav>

      {/* Footer */}
      <div style={{ padding: '16px 20px', borderTop: '1px solid var(--border)', fontSize: 11, color: 'var(--text2)' }}>
        Resonance Liminal v2.0
      </div>
    </aside>
  );
}
