from setuptools import find_packages
import pathlib
from numpy.distutils.core import setup, Extension

HERE=pathlib.Path(__file__).parent
README=(HERE / "README.md").read_text()

ext1 = Extension(name='nc_fort',
                 sources=['nc_cryst/nc_fort.f90'],
                 f2py_options=['--quiet'],
                )

setup(name="nc_cryst-CASTEP",
      version="0.1.33",
      #package_dir={"": "nc_cryst"},
      packages=["nc_cryst"],#find_packages(where="nc_cryst"),
      ext_modules=[ext1],
      description="Crystal visulaliser",
      long_description=README,
      long_description_content_type="text/markdown",
      url="https://github.com/zachary-hawk/NC_CRYST.git",
      author="Zachary Hawkhead",
      author_email="zachary.hawkhead@durham.ac.uk",
      license="MIT",
      classifiers=[
          "License :: OSI Approved :: MIT License",
          "Programming Language :: Python :: 3",
          "Programming Language :: Python :: 3.7",
      ],
      include_package_data=True,
      install_requires=["numpy","matplotlib","scipy","ase","pyvista","vtk","argparse"],
      entry_points={"console_scripts":["nc_cryst=nc_cryst.__main__:main",]
      }

      )

