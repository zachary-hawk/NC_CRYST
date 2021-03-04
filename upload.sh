#!/bin/bash


rm -f dist/*
rm -f -r build/*
python setup.py sdist bdist_wheel
twine upload --repository-url https://test.pypi.org/legacy/ dist/* 
sleep 15
pip install --upgrade -i https://test.pypi.org/simple/ nc-cryst-CASTEP
pip install --upgrade -i https://test.pypi.org/simple/ nc-cryst-CASTEP
