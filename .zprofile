export DICTIONARY=russian-aot,en_US

export LANGUAGE=en_US.UTF-8
export LANG=en_US.UTF-8

if locale -a | grep ru_RU.UTF-8 > /dev/null; then
    export LC_TIME=ru_RU.UTF-8
    export LC_NUMERIC=ru_RU.UTF-8
    export LC_COLLATE=ru_RU.UTF-8
    export LC_CTYPE=ru_RU.UTF-8
    export LC_MEASUREMENT=ru_RU.UTF-8
fi

export PATH=$PATH:/usr/local/bin:$HOME/.local/bin:$HOME/.ghcup/bin
export PATH="/opt/local/bin:/opt/local/sbin:$PATH"

if [[ -d "/opt/homebrew/etc/ca-certificates/" ]]; then
    export SSL_CERT_FILE="/opt/homebrew/etc/ca-certificates/cert.pem"
    export CURL_CA_BUNDLE="/opt/homebrew/etc/ca-certificates/cert.pem"
    export REQUESTS_CA_BUNDLE="/opt/homebrew/etc/ca-certificates/cert.pem"
    export NODE_EXTRA_CA_CERTS="/opt/homebrew/etc/ca-certificates/cert.pem"
fi
