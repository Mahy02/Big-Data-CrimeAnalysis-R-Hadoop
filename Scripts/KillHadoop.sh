#!/bin/bash

jps | awk '{print $1}' | xargs kill
