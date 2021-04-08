#/bin/sh

export DEST=/media/astynax/backup/astynax

if [ ! -d "$DEST" ]; then
    echo "Please, attach the backup storage"
    exit 1
fi

cd "$HOME"

rsync -Paub \
      Downloads \
      Documents \
      Dropbox \
      Pictures \
      Videos \
      Yandex.Disk \
      .software \
      $DEST

rsync -Paub \
      --exclude=.stack-work/ \
      --exclude=dist-newstyle/ \
      --exclude=elm-stuff/ \
      --exclude=target/ \
      --exclude=compiled/ \
      --exclude=.venv/ \
      Projects \
      $DEST
