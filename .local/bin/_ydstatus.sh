#!/usr/bin/bash

set -euo pipefail

case "$*" in
    "-h"|"--help")
        cat <<EOF
This is a companion script for the xfce4-genmon-plugin.
It provides an icon for the widget, a tooltip for it
and a menu to control the Yandex.Disk daemon.

Usage:
    $0 [menu|-h|--help]
EOF
        ;;
    "menu")
        MSG=$(echo "status|start|stop" \
                  | rofi -p "Yandex.Disk" \
                         -dmenu -sep '|' -no-config -location 5 \
                  | xargs -r -I{} 2>/dev/null yandex-disk {}) || true
        if [ ! -z "$MSG" ]; then
            notify-send "$MSG"
        fi
        ;;
    *)
        ICONS=/usr/share/icons/Adwaita/scalable

        STATUS=$(yandex-disk status)
        STATE=$(echo "$STATUS" | head -n 1 | sed -e 's/^[^:]*: //')

        echo -e "<tool>Yandex.Disk\n\n$STATUS</tool>"

        case "$STATE" in
            "ожидание команды")
                echo "<img>$ICONS/emblems/emblem-default-symbolic.svg</img>"
                ;;
            "оработка данных")
                echo "<img>$ICONS/emblems/emblem-synchronizing-symbolic.svg</img>"
                ;;
            "остановлен")
                echo "<img>$ICONS/emotes/face-sick-symbolic.svg</img>"
                ;;
            *)
                echo "<img>$ICONS/emblems/emblem-important-symbolic.svg</img>"
                ;;
        esac
        echo "<click>$0 menu</click>"
        ;;
esac
