from setuptools import find_packages
from setuptools import setup

setup(name="nc_cryst",
      version="1.0.0",
      packages=find_packages(),
      description="CASTEP utility for visualising crystals and displaying non-collinear magnetism.",
      url="https://github.com/zachary-hawk/NC_CRYST.git",
      author="Zachary Hawkhead",
      author_email="zachary.hawkhead@durham.ac.uk",
      license="MIT",
      install_requires=["numpy",
                        "matplotlib",
                        "scipy",
                        "ase",
                        "pyvista",
                        "vtk",
                        "argparse",
                        "warnings",
                        "itertools"],
      entry_points={"console_scripts":["nc_cryst=nc_cryst.main:main",]
      }

      )

