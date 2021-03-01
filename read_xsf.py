import numpy as np
from scipy.interpolate import RegularGridInterpolator

def read_xsf(file):
    scale=1
    xsf=open(file,'r')

    lines=xsf.readlines()
    for i in range(len(lines)):
        if lines[i].find("BEGIN_DATAGRID_3D")!=-1:
            break
    nx,ny,nz=np.array((lines[i+1].split()),dtype=int)

    start=i+6
    mesh=np.zeros((nx,ny,nz))
    n=0
    for j in range(0,nx):
        for k in range(0,ny):
            for l in range(0,nz):
                mesh[j,k,l]=np.float(lines[start+n])
                n+=1
    '''
    x=np.linspace(0,nx,nx)
    y=np.linspace(0,ny,ny)
    z=np.linspace(0,nz,nz)
    
    f=RegularGridInterpolator((x,y,z),mesh)
    nx=scale*nx
    ny=scale*ny
    nz=scale*nz
    mesh_flat=np.zeros((nx,ny,nz))
    for j in range(0,nx):
        for k in range(0,ny):
            for l in range(0,nz):
                mesh_flat[j,k,l]=f((j/scale,k/scale,l/scale))'''

    return nx,ny,nz,mesh.flatten()
