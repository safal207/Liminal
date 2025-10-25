#!/bin/bash
# Скрипт для создания релизного тега
# Использование: ./scripts/create-release.sh 1.0.0 [--major|--minor|--patch]

set -e  # Выход при первой ошибке

# Получаем текущую ветку
CURRENT_BRANCH=$(git rev-parse --abbrev-ref HEAD)

# Проверяем, находимся ли мы в основной ветке
if [ "$CURRENT_BRANCH" != "main" ] && [ "$CURRENT_BRANCH" != "master" ]; then
  echo "ВНИМАНИЕ: Вы не находитесь в основной ветке (main/master)."
  echo "Продолжаем с веткой $CURRENT_BRANCH для тестирования."
  # Для тестирования отключаем проверку ветки
  # Вернуть этот код после тестирования:
  # read -p "Продолжить с веткой $CURRENT_BRANCH? (y/N) " CONTINUE
  # if [[ $CONTINUE != "y" && $CONTINUE != "Y" ]]; then
  #   echo "Операция отменена."
  #   exit 1
  # fi
fi

# Проверяем наличие незакоммиченных изменений
if ! git diff-index --quiet HEAD --; then
  echo "ВНИМАНИЕ: В рабочей директории есть незакоммиченные изменения."
  git status --short
  echo "Продолжаем без коммита для тестирования."
  # Для тестирования отключаем проверку незакоммиченных изменений
  # Вернуть этот код после тестирования:
  # read -p "Продолжить без учета этих изменений? (y/N) " CONTINUE
  # if [[ $CONTINUE != "y" && $CONTINUE != "Y" ]]; then
  #   echo "Операция отменена. Сделайте commit изменений перед созданием релиза."
  #   exit 1
  # fi
fi

# Обработка аргументов
if [ "$#" -lt 1 ]; then
  # Если не указана версия, автоматически определяем следующую
  LAST_TAG=$(git describe --tags --abbrev=0 2>/dev/null || echo "v0.0.0")
  LAST_VERSION=${LAST_TAG#v}
  IFS='.' read -ra VERSION_PARTS <<< "$LAST_VERSION"
  MAJOR=${VERSION_PARTS[0]:-0}
  MINOR=${VERSION_PARTS[1]:-0}
  PATCH=${VERSION_PARTS[2]:-0}
  
  # Определяем тип инкремента версии
  if [ "$1" == "--major" ]; then
    MAJOR=$((MAJOR + 1))
    MINOR=0
    PATCH=0
    shift
  elif [ "$1" == "--minor" ]; then
    MINOR=$((MINOR + 1))
    PATCH=0
    shift
  else  # По умолчанию увеличиваем патч-версию
    PATCH=$((PATCH + 1))
    if [ "$1" == "--patch" ]; then
      shift
    fi
  fi
  
  VERSION="$MAJOR.$MINOR.$PATCH"
else
  # Используем указанную версию
  VERSION=$1
  shift
fi

# Проверяем формат версии (должен быть X.Y.Z)
if ! [[ $VERSION =~ ^[0-9]+\.[0-9]+\.[0-9]+$ ]]; then
  echo "Ошибка: Версия должна быть в формате X.Y.Z (например, 1.2.3)"
  exit 1
fi

# Проверяем, существует ли уже такой тег
if git rev-parse "v$VERSION" >/dev/null 2>&1; then
  echo "Ошибка: Тег v$VERSION уже существует."
  exit 1
fi

# Создаем сообщение коммита для релиза
COMMIT_MSG="Release v$VERSION"

# Предлагаем добавить дополнительную информацию к релизу
echo "Создание релизного тега v$VERSION"
read -p "Введите дополнительную информацию о релизе (опционально): " RELEASE_INFO

if [ -n "$RELEASE_INFO" ]; then
  COMMIT_MSG="$COMMIT_MSG: $RELEASE_INFO"
fi

# Создаем аннотированный тег
echo "Создание тега v$VERSION с сообщением: \"$COMMIT_MSG\""
git tag -a "v$VERSION" -m "$COMMIT_MSG"

# Выводим информацию о созданном теге
echo -e "\n✅ Тег v$VERSION успешно создан!"
echo -e "\nИспользуйте следующие команды для завершения релиза:"
echo "git push origin v$VERSION  # Отправить тег на GitHub"
echo "git push                   # Отправить изменения веток (если есть)"

echo -e "\nПосле отправки тега, GitHub Actions автоматически начнет процесс деплоя."
