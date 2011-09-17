#!/bin/sh
export CLASSPATH
CLASSPATH=classes:../infobus.jar:../lib/methodtracer.jar
java sun.beanbox.BeanBoxFrame
