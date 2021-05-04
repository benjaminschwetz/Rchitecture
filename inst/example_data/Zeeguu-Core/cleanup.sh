#!/bin/bash
sudo rm -rf build/
sudo rm -rf dist/
sudo rm -rf zeeguu.egg-info/
find . | grep pyc | xargs rm -rf

