from setuptools import find_packages

from numpy.distutils.core import setup, Extension

ext1 = Extension(name='nc_fort.modf90',
                 sources=['src/nc_fort.f90'],
                 f2py_options=['--quiet'],
                )

setup(name="nc_cryst",
      version="0.1.0",
      package_dir={"": "src"},
      packages=find_packages(where="src"),
      ext_modules=[ext1],
      description="Read the latest Real Python tutorials",
      long_description=README,
      long_description_content_type="text/markdown",
      url="https://github.com/zachary-hawk/NC_CRYST.git",
      author="Zachary Hawkhead",
      author_email="info@realpython.com",
      license="MIT",
      classifiers=[
          "License :: OSI Approved :: MIT License",
          "Programming Language :: Python :: 3",
          "Programming Language :: Python :: 3.7",
      ],
      include_package_data=True,
      install_requires=["numpy","matplotlib","scipy","sys","ase","pyvista","vtk","tkinter","time","warnings","argparse","time","colorsys","os","datetime"],
      entry_points={
          "console_scripts": [
              "realpython=reader.__main__:main",
          ]
      },
      )
