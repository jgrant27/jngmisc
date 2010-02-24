#!/usr/bin/env bash

rm -fr classes/clojure/proxy/javax/swing/JFrame\$ActionListener\$KeyListener\$MouseMotionListener.class
rm -fr classes/i27/
rm pong.jar

echo "(load-file \"`pwd`/src/i27/games/pong.clj\") (compile 'i27.games.pong)" | clj

pushd classes
jar cfm ../pong.jar manifest.txt clojure/* i27/games/pong*
popd

#java -jar pong.jar
