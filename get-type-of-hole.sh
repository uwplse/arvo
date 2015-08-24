#!/bin/bash

$ARVO_HOME/arvo $1 2>&1 | grep "Hole has type" | head -n1 | cut -d' ' -f9- | tr -d '\n'
