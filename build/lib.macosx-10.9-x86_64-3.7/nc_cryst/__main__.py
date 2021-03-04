#!/usr/bin/env python3
import numpy as np
from nc_cryst import read_xsf as xsf
import matplotlib.pyplot as plt
import sys,os
from scipy.interpolate import RegularGridInterpolator
import scipy.interpolate as interpolate
import pyvista as pv
import warnings
import time
import argparse
from colorsys import rgb_to_hsv, hsv_to_rgb
import datetime
from matplotlib.colors import LinearSegmentedColormap
import vtk
from pyvista.plotting.theme import parse_color
import ase.io as io
from ase.data.colors import jmol_colors
from ase.data.vdw_alvarez import vdw_radii
from ase.build import cut,make_supercell
from ase.calculators.castep import Castep
import ase.spacegroup as spg
import time
from nc_fort import nc_fort
from  tkinter import *


def main():

    datenow = datetime.datetime.now()
    datenow=datenow.strftime("%d/%m/%Y %H:%M:%S")
    sys.argv[0]=sys.argv[0].replace(" ","\ ")
    start=time.perf_counter()
    
    # Write a log file
    log=open("nc_cryst.log","a+")
    log_line=" ".join(sys.argv)
    log.write(datenow+" "+log_line+"\n")
    log.close()
    
    
    warnings.filterwarnings("ignore")
    
    
    
    
    # Disable
    ##################################################################################
    
    def blockPrint():
        sys.stdout = open(os.devnull, 'w')
    
    # Restore
    def enablePrint():
        sys.stdout = sys.__stdout__
    
    def complementary(hex):
       """returns RGB components of complementary color"""
       hex = hex.lstrip('#')
       r,g,b= tuple(int(hex[i:i+2], 16) for i in (0, 2, 4))
    
       hsv = rgb_to_hsv(r, g, b)
    
       return (r/255,g/255,b/255),tuple(np.array(hsv_to_rgb(((hsv[0] + 0.5) % 1), hsv[1], hsv[2]))/255)
    
    
    def update_axes_label_color(axes_actor, color=None):
        """Set the axes label color (internale helper)."""
        if color is None:
            color = rcParams['font']['color']
        color = parse_color(color)
        if isinstance(axes_actor, vtk.vtkAxesActor):
            prop_x = axes_actor.GetXAxisCaptionActor2D().GetCaptionTextProperty()
            prop_y = axes_actor.GetYAxisCaptionActor2D().GetCaptionTextProperty()
            prop_z = axes_actor.GetZAxisCaptionActor2D().GetCaptionTextProperty()
            for prop in [prop_x, prop_y, prop_z]:
                prop.SetColor(color[0], color[1], color[2])
                prop.SetShadow(False)
        elif isinstance(axes_actor, vtk.vtkAnnotatedCubeActor):
            axes_actor.GetTextEdgesProperty().SetColor(color)
    
        return
    
    
    def create_axes_marker2(label_color=None, x_color=None, y_color=None,
                           z_color=None, xlabel='a', ylabel='b', zlabel='c',
                            labels_off=False, line_width=50):
        """Return an axis actor to add in the scene."""
        if x_color is None:
            x_color = rcParams['axes']['x_color']
        if y_color is None:
            y_color = rcParams['axes']['y_color']
        if z_color is None:
            z_color = rcParams['axes']['z_color']
        axes_actor = vtk.vtkAxesActor()
    
        axes_actor.GetXAxisShaftProperty().SetColor(parse_color(x_color))
        axes_actor.GetXAxisTipProperty().SetColor(parse_color(x_color))
        axes_actor.GetYAxisShaftProperty().SetColor(parse_color(y_color))
        axes_actor.GetYAxisTipProperty().SetColor(parse_color(y_color))
        axes_actor.GetZAxisShaftProperty().SetColor(parse_color(z_color))
        axes_actor.GetZAxisTipProperty().SetColor(parse_color(z_color))
        
        transform=vtk.vtkTransform()
        mat=transform.GetMatrix()
        latt_or=np.array(latt)
        latt_or[:,0]=2*latt_or[:,0]/np.linalg.norm(latt_or[:,0])
        latt_or[:,1]=2*latt_or[:,1]/np.linalg.norm(latt_or[:,1])
        latt_or[:,2]=2*latt_or[:,2]/np.linalg.norm(latt_or[:,2])
        for i in range(len(latt)):
            for j in range(len(latt)):
                mat.SetElement(i,j,2*latt_or[i,j])
    
        axes_actor.SetUserTransform(transform)
    
        text=vtk.vtkTextProperty()
        text.SetFontSize(100)
        text.SetBold(True)
        text.SetFontFamilyAsString("Times")
        
        # Set labels
        axes_actor.SetXAxisLabelText(xlabel)
        axes_actor.SetYAxisLabelText(ylabel)
        axes_actor.SetZAxisLabelText(zlabel)
        axes_actor.SetNormalizedLabelPosition((1.3,1.3,1.3))
        axes_actor.GetXAxisCaptionActor2D().SetCaptionTextProperty(text)
        axes_actor.GetYAxisCaptionActor2D().SetCaptionTextProperty(text)
        axes_actor.GetZAxisCaptionActor2D().SetCaptionTextProperty(text)
    
        
        if labels_off:
            axes_actor.AxisLabelsOff()
        # Set Line width
        axes_actor.GetXAxisShaftProperty().SetLineWidth(line_width)
        axes_actor.GetYAxisShaftProperty().SetLineWidth(line_width)
        axes_actor.GetZAxisShaftProperty().SetLineWidth(line_width)
        #axes_actor.SetNormalizedTipLength(1,1,1)
        #axes_actor.SetNormalizedShaftLength(2,2,2)
    
        update_axes_label_color(axes_actor, label_color)
    
        #axes_actor.SetNormalizedShaftLength(1.6,1.6,1.6)
        #axes_actor.SetNormalizedTipLength(0.4,0.4,0.4)
        #axes_actor.SetTotalLength(2,2,2)
        return axes_actor
    
    
    #atomic valency
    valency=np.array([1,0,1,2,3,4,5,8,1,0,1,2,3,4,5,6,6,
             0,1,2,3,6,5,6,7,6,5,6,4,2,3,4,5,6,
             7,0,1,2,3,4,5,6,7,8,6,4,3,2,3,4,5,
             6,7,8,1,2,3,4,4,4,3,3,3,3,3,4,4,3,
             3,3,3,3,4,5,6,7,8,6,6,7,2,3,4,5,6,
             4,7,0,1,2,3,4,5,6,7,7,7,6,4,5,4,4,
             3,3,3,4,5,6,7,8,6,6,3,2,1,2,3,4,0,8])
    
    # Some functions
    
    track_r=np.array([[-10,-10,-10]])
    def field_lines(point_grid):
        #ds=(V**(1/3))/N
        #ads=2/N
        mag_2=[]
        N=100
        #ds=0.01
        track_n=[]
        track_r=np.array([[-10,-10,-10]])
        
        lines=np.zeros((2*len(point_grid),N,3))
        mag=np.zeros((2*len(point_grid),N))
        j=-1
        for o,r0 in enumerate(point_grid):
            F_old=[0,0,0]
            F_mag=0
            for one in [1,-1]:
                j+=1
                xs=[]
                ys=[]
                zs=[]
           
    
                r=r0
                for n in range(N):
                    s=0.01
                    xs.append(r[0])
                    ys.append(r[1])
                    zs.append(r[2])
                    lines[j,n,:]=r
                    mag[j,n]=np.linalg.norm(F_mag)
                    #mag.append(np.linalg.norm(F_mag))
                    #mag_2.append(np.linalg.norm(F_mag))
                    if nc_fort.is_close(track_r,r,len(track_r),5e-3):                
                        break
    
                    track_r=np.vstack((track_r,r))
    
                    if r[0]>=np.max(X) or r[0] <= np.min(X) or r[1] >= np.max(Y) or r[1] <= np.min(Y) or r[2] >= np.max(Z) or r[2] <= np.min(Z):
    
                        break
                    x,y,z=r
    
                    try:
                        vec_x=f_x([x,y,z])
                    except:
                        vec_x=[0,0,0]
    
                    try:
                        vec_y=f_y([x,y,z])
                    except:
                        vec_y=[0,0,0]
                    
                    try:
                        vec_z=f_z([x,y,z])
                    except:
                        vec_z=[0,0,0]
    
                    
                    F=np.array([vec_x[0],vec_y[0],vec_z[0]])#line_gen(r)
                    F_mag=F
                    F=F/np.linalg.norm(F)
                    phi=np.dot(F,F_old)
                    phi=np.arccos(phi)/np.pi
                    #print(n,phi)
                    if np.round(phi,4)==1:
                        break
                    
                    r=r+one*F*s
                    F_old=F
                track_n.append(n)
                xs=np.array(xs)
                ys=np.array(ys)
                zs=np.array(zs)
    
                line_points=np.column_stack((xs,ys,zs))

        return lines,mag,track_n
    
    
    #############################################################
    
    
    
    
    
    #import matplotlib.pyplot as plt
    
    
    
    
    # LEFT BLANK
    
    
    
    
    
    
    
    
    
    
    ###############################################################
    
     ### #######          ######                                     
      #  #     #          #     #   ##   #####   ####  ###### #####  
      #  #     #          #     #  #  #  #    # #      #      #    # 
      #  #     # #####    ######  #    # #    #  ####  #####  #    # 
      #  #     #          #       ###### #####       # #      #####  
      #  #     #          #       #    # #   #  #    # #      #   #  
     ### #######          #       #    # #    #  ####  ###### #    # 
                                                                     
    ################################################################
    
    # We want to read the parameters from a file called seed.nc_param
    
    # get from the commandline the seed
    parser = argparse.ArgumentParser(description= "Visualisation of cell structures and non-collinear magentic properties from a CASTEP job.")
    parser.add_argument("seed",help="The seed from the CASTEP calculation.")
    
    
    parser.add_argument("-v","--verbose",action="store_true",help="Turn on verbose output.")
    #parser.add_argument("-s","--sym",help="Tolerance for specifying reproduction of atoms outside unit cell (Ang)",default=1)
    parser.add_argument("-i","--initmag",action="store_true",help="Plot initial magnetic moment vectors.")
    parser.add_argument("-c","--castep",action="store_true",help="Read <seed>.castep file to determine moments. Only for NCM calculation (BETA)")
    parser.add_argument("-f","--field",help="Read formatted potential or density to produce field. Only from VECTOR magnetic run.",action="store_true")
    parser.add_argument("-o","--orient",help="Orientation of the crystal structure, takes values 'a,b,c,a*,b*,c*'.",default="sd")
    parser.add_argument("-B","--bond",help="Set maximun bond length.",default=2.4)
    parser.add_argument("--save",help="Save image.",action="store_true")
    parser.add_argument("-d","--delete",help="Delete atoms",nargs='+')
    parser.add_argument("-p","--position",help="Camera position vector",nargs=6,default=np.array([0.,0.,0.,0.,0.,0.]))
    parser.add_argument("-V","--volumetric",help="Provide file with volumetric data: .xsf .den_fmt .pot_fmt accepted.")
    parser.add_argument("-I","--iso",help="Isosurface value for volumetric data",nargs="*")
    parser.add_argument("--colour",help="HEX code for Isosurface colouring",default="#0000FF")
    parser.add_argument("-z","--zoom",help="Zoom multiplier",default=1)
    parser.add_argument("-e","--exclude",help="Exclude atoms outside first unitcell",action="store_false")
    parser.add_argument("-l","--lines",help="Disable plotting of field lines of a provoded field line",action="store_true")
    parser.add_argument("-P","--plane",help="Three points in fractional coordinates to define a plane for B-field.",nargs="*")
    parser.add_argument("-w","--widget",help="Disable interactive widgets",action="store_false")
    parser.add_argument("-s","--saturation",help="Saturation level for sections.", default=1)
    parser.add_argument("-S","--spin",help="Plot spin isosurfaces from .den_fmt",action="store_true")
    parser.add_argument("-C","--charge",help="Plot charge isosurfaces from .den_fmt",action="store_true")
    parser.add_argument("-r","--reduction",help="Factor used to reduce the size of atoms, useful for visualising volumetric data without loss of context.",default=1.0)
    args = parser.parse_args()
    seed = args.seed
    #do_legend = args.legend
    do_verbose= args.verbose
    do_init_mag=args.initmag
    do_magmom=args.castep
    #do_Bfield=args.B_XC
    field=args.field
    #sym_tol=np.float(args.sym)
    orient=args.orient
    bond_cut=np.float(args.bond)
    save=args.save
    hide=args.delete
    cam_pos=args.position
    z=np.float(args.zoom)
    hide_lines=args.lines
    plane=args.plane
    exclude=args.exclude
    widgets=args.widget
    sat=np.float(args.saturation)
    docharge=args.charge
    dospin=args.spin
    reduction=np.float(args.reduction)
    iso=args.iso
    
    if plane==None:
        do_plane=False
    
    elif len(plane)==9:
        do_plane=True
    elif len(plane)==0:
        do_plane=True
        widgets=True
    else:
        print("Insufficient points provided for plane")
        sys.exit()
    
    
    if iso==None:
        do_iso=False
    elif len(iso)==0:
        do_iso=True
        iso=None
    elif len(iso)>1:
        print("Incorrect number of ISO arguments")
        sys.exit()
    else:
        do_iso=True
        
        
    
    # Make Charge the default
    if not docharge and not dospin:
        docharge=True
    if dospin:
        docharge=False
        
        
    xsf_file=args.volumetric
    hex_col=args.colour
    sym_tol=bond_cut
    
    
    for i in range(len(cam_pos)):
        cam_pos[i]=np.float(cam_pos[i])
    
    
    
    if hide==None:
        hide=[""]
    
    
    # Define all the options (and the defaults)
    do_bonds = True
    #do_magmom = False
    #do_Bfield = False
    do_proj = False
    h=0.5
    #b_xc_file=seed+".B_xc.pot_fmt"
    
    xsffile=False
    denfile=False
    potfile=False
    noncollinear=False
    
    # Set iso surface colourmap
    iso_colours=complementary(hex_col)
    colors=list(iso_colours)
    colours=[colors[1],colors[0]]
    cmap_name ="iso_colors"
    cm = LinearSegmentedColormap.from_list(
        cmap_name, colours, N=2)
    
    
    
    # Open the tkinter window
    window=Tk()
    window.resizable(False, False)
    window.title("NC_CRYST: "+seed)
    
    output=Text(window)
    output.grid(row=1,column=0,columnspan=4)#,sticky=N+S+W)
    
    
    
    ##################################################################
    
    
    
    
    
    
    
    
    
    # Define all of the atom positions
        
    blockPrint()
    #if do_verbose:
    #    print("Parsing .cell")
    
    cell=io.read(seed+".cell")
    ccalc = Castep()
    ase_cell=cell.get_cell()
    a,b,c,alpha,beta,gamma=cell.get_cell_lengths_and_angles()
    pos=cell.get_positions()
    prim_pos=pos
    cell.set_calculator(ccalc)
    latt=np.transpose(np.matmul(np.identity(3),cell.get_cell()))
    
    
    
    init_mag=np.zeros((len(pos),3))
    init_spin=np.zeros((len(pos),3))
    if do_init_mag:
        try:
            with open(seed+".cell") as init_cell:
                data=init_cell.readlines()
        except:
            print("No file: "+seed+".cell")
            sys.exit()
        pos_i=[]
    
        counter=0
        for i in data:
            if "spin" in i.lower():
                
                try:
                    i=i.replace("="," ")
                    i=i.strip("\n")
                except:
                    None
    
                i=i.split()
                if len(i)>6:
                    pos_i.append([np.float(j) for j  in i[1:4]])
                    init_mag[counter]=[np.float(j) for j  in i[5:8]]
                else:
                    pos_i.append([np.float(j) for j  in i[1:4]])
                    temp=[0. , 0., np.float(i[5])]
                    init_mag[counter]=temp
                counter+=1
        init_spin=np.zeros((len(pos),3))
        sum_dat="".join(data).lower()
        for i in range(len(pos_i)):
            for j in range(len(pos)):
                if "positions_frac" in sum_dat:
                    dist=np.sum((np.matmul(latt,pos_i[i])-pos[j])**2)
                    if dist<0.00001:
                        init_spin[j]=init_mag[i]
                else:
                    dist=np.sum((pos_i[i]-pos[j])**2)
                    if dist<0.00001:
                        init_spin[j]=init_mag[i]
    
    
    cell.set_velocities(init_spin)
    
    
    # Make the supercell (gets all of the positions for me)
    scell=make_supercell(cell,3*np.identity(3))
    
    pos=scell.get_positions()
    atoms=scell.get_atomic_numbers()
    symb=scell.get_chemical_symbols()
    Vol=cell.get_volume()
    enablePrint()
    #print(pos)
    
    
    atom_colours=jmol_colors[atoms]
    atom_radii=vdw_radii[atoms]

    inv_latt=np.linalg.inv(np.array(ase_cell.T))
    init_spin=scell.get_velocities()
    #prim_count=0
    #prim_list=np.zeros(len(pos),order="F")
    

    
    
    
    
    
    
    prim_count,prim_list,pos,keep,n_atoms,bonds,n_bonds=nc_fort.sym_positions(bond_cut,len(pos),pos,latt,inv_latt,exclude)
    
    prim_list=np.array(keep[0:prim_count])
    
    #sys.exit()
    
    
    pos=pos[prim_list]
    atoms=atoms[prim_list]
    #pos=pos[prim_list]
    atom_radii=atom_radii[prim_list]
    atom_colours=atom_colours[prim_list]
    symb=np.array(symb)[prim_list]
    init_spin=init_spin[prim_list]
    
    
    
    unique_atom,atom_counts=np.unique(atoms,return_counts=True)
    atom_label=[]
    sort=[]
    
    ###################################### SYMMETRY POSITIONS ###################
    for j in unique_atom:
        for i in range(len(cell.get_atomic_numbers())):
            if atoms[i]==j:
                sort.append(i)
    
    sort=np.array(sort)
    
        
    if do_magmom:
    
        with open(seed+".castep") as castep:
            castep_lines=castep.readlines()
    
        for no,test in enumerate(castep_lines):
            if "Noncollinear Spin Vectors" in test:
                line_no=no
        mom_vec=[]
        vec_symb = []
        for i in range(line_no+4,line_no+4+len(prim_pos)):
            cline=castep_lines[i]
            cline=cline.split()
            vec=[np.float(cline[2]),np.float(cline[3]),np.float(cline[4])]
            vec_symb.append(cline[0])
            mom_vec.append(vec)
        #print(mom_vec)
        vec_symb_2=list(vec_symb)
        mom_vec_2=list(mom_vec)
        for i in range(len(sort)):
            vec_symb_2[sort[i]]=vec_symb[i]
            mom_vec_2[sort[i]]=mom_vec[i]
            
        vec_symb=list(vec_symb_2)
        mom_vec=list(mom_vec_2)
    
    if do_magmom:
        ccell=cell.copy()
        ccell.set_velocities(mom_vec)
        ccell=make_supercell(ccell,3*np.identity(3))
        mom_vec=ccell.get_velocities()
    
    
    sort=np.argsort(atoms)
    
    
    atoms=atoms[sort]
    pos=pos[sort]
    atom_radii=atom_radii[sort]
    atom_colours=atom_colours[sort]
    symb=np.array(symb)[sort]
    if do_init_mag:
        init_spin=init_spin[sort]
    if do_magmom:
        mom_vec=np.array(mom_vec)
        mom_vec=mom_vec[sort]
        
    for i in atom_counts:
        for j in range(i):
            atom_label.append(j+1)
    
    
    
    hide_num=[]
    for i in range(len(symb)):
        for j in hide:
            if j==symb[i]:
                hide_num.append(i)
    
    
    
    
                
    
    
    
    
    
    
            
    
        
    # Time for some plotting
    
    
    pv.set_plot_theme("document")
    if save:
        p=pv.Plotter(off_screen=True) 
    else:
        p = pv.Plotter()
    p.enable_parallel_projection()
    
    
    orientation=[latt[:,0],latt[:,1],latt[:,2]]
    for i in range(3):
        orientation[i]=orientation[i]/2*np.linalg.norm(orientation[i])
    arrow_or=pv.Arrow([0,0,0],orientation[0],tip_length=0.25, tip_radius=0.09, tip_resolution=20, shaft_radius=0.03, shaft_resolution=20) +\
        pv.Arrow([0,0,0],orientation[1],tip_length=0.25, tip_radius=0.09, tip_resolution=20, shaft_radius=0.03, shaft_resolution=20) +\
        pv.Arrow([0,0,0],orientation[2],tip_length=0.25, tip_radius=0.09, tip_resolution=20, shaft_radius=0.03, shaft_resolution=20)+\
        pv.Sphere(0.1,[0,0,0])  
    
    
    test=create_axes_marker2(
                    label_color="black", line_width=4,
                    x_color="r", y_color="g", z_color="b",
                    xlabel="a", ylabel="b", zlabel="c", labels_off=False)
    p.add_orientation_widget(test)
    
    
    ########################################################################################################################
    # _______ _       _     _     ______      _             _            _                  
    #(_______|_)     | |   | |   / _____)    | |           | |      _   (_)                 
    # _____   _  ____| | _ | |  | /      ____| | ____ _   _| | ____| |_  _  ___  ____   ___ 
    #|  ___) | |/ _  ) |/ || |  | |     / _  | |/ ___) | | | |/ _  |  _)| |/ _ \|  _ \ /___)
    #| |     | ( (/ /| ( (_| |  | \____( ( | | ( (___| |_| | ( ( | | |__| | |_| | | | |___ |
    #|_|     |_|\____)_|\____|   \______)_||_|_|\____)\____|_|\_||_|\___)_|\___/|_| |_(___/ 
    ########################################################################################################################
    
    
    if xsf_file!=None:
    
        # See what sort of file we have.
        if ".den_fmt" in xsf_file:
            denfile=True
        elif ".pot_fmt" in xsf_file:
            potfile=True
        elif ".xsf" in xsf_file:
            xsffile=True
            nx,ny,nz,mesh_mag=xsf.read_xsf(xsf_file)
    
        
    
        if potfile:
            docharge=False
            dospin=False
        if potfile or denfile:
            with open(xsf_file) as header:
                data=header.readlines()[0:11]
    
    
            nx,ny,nz = data[8].split()[0:3]
            nx,ny,nz=int(nx),int(ny),int(nz)
    
            #latt=np.array([data[3].split()[0:3],data[4].split()[0:3],data[5].split()[0:3]]).astype(float)
    
    
            #latt=np.transpose(latt)
            V = np.loadtxt(xsf_file,skiprows=11)
    
    
            #n= np.round(h*nz)
    
            #mask=(V[:,2] == n )
            V3D=V
            #V=V[mask]
    
        
            if potfile:
                if np.shape(V3D)[1]==9:
                    noncollinear=True
                    V1= V3D[:,3]+1j*V3D[:,4]
                    V2= V3D[:,5]+1j*V3D[:,6]
                    V3= V3D[:,7]+1j*V3D[:,8]
                    
                    
                    B_x=np.real((V3+np.conj(V3))/2)
                    B_y=np.real(1j*(V3-np.conj(V3))/2)
                    B_z=np.real((V1-V2)/2)
                else:
                    noncollinear=False
                    print("3D data only accepted for NCM .pot_fmt, Exiting...")
                    sys.exit()
                    
            if denfile:
                if np.shape(V3D)[1]==7:            
                    noncollinear=True
                    B_x=V3D[:,4]
                    B_y=V3D[:,5]
                    B_z=V3D[:,6]
                    charge=V3D[:,3]
                    mesh_mag=np.zeros((nx,ny,nz))
                    if docharge:
                        for i in range(len(V3D)):
                            mesh_mag[int(V3D[i,0]-1),int(V3D[i,1]-1),int(V3D[i,2]-1)]=charge[i]
                else:
                    noncollinear=False
                    charge=V3D[:,3]
                    spin=V3D[:,4]
                    mesh_mag=np.zeros((nx,ny,nz))
                    if docharge:
                        for i in range(len(V3D)):
                            mesh_mag[int(V3D[i,0]-1),int(V3D[i,1]-1),int(V3D[i,2]-1)]=charge[i]
                    if dospin:
                        for i in range(len(V3D)):
                            mesh_mag[int(V3D[i,0]-1),int(V3D[i,1]-1),int(V3D[i,2]-1)]=spin[i]
                mesh_mag=mesh_mag/(nx*ny*nz)
    
        #print("NCM: ",noncollinear)
        #print("POT: ",potfile)
        #print("DEN: ",denfile)
        #print("Charge: ",docharge)
        #print("Spin: ",dospin)
        if noncollinear and not docharge:    
                
        
            mesh3D_x=np.zeros((nx,ny,nz))
            mesh3D_y=np.zeros((nx,ny,nz))
            mesh3D_z=np.zeros((nx,ny,nz))
    
    
            for i in range(len(V3D)):
                mesh3D_x[int(V3D[i,0]-1),int(V3D[i,1]-1),int(V3D[i,2]-1)]=B_x[i]
                mesh3D_y[int(V3D[i,0]-1),int(V3D[i,1]-1),int(V3D[i,2]-1)]=B_y[i]
                mesh3D_z[int(V3D[i,0]-1),int(V3D[i,1]-1),int(V3D[i,2]-1)]=B_z[i]
            mesh_mag=np.sqrt(mesh3D_x**2+mesh3D_y**2+mesh3D_z**2)
    
    
            # Calcuate the Div
            x_flux=np.sum(mesh3D_x[0:-1,0,0])-np.sum(mesh3D_x[0:-1,-1,-1])
            y_flux=np.sum(mesh3D_x[0,0:-1,0])-np.sum(mesh3D_x[-1,0:-1,-1])
            z_flux=np.sum(mesh3D_x[0,0,0:-1])-np.sum(mesh3D_x[-1,-1,0:-1])
    
            flux=x_flux+y_flux+z_flux
    
            
            # Play here might cause asymmetry
            Vx=(V[:,0]-1)/(nx)
            Vy=(V[:,1]-1)/(ny)
            Vz=(V3D[:,2]-1)/(nz)
            
    
            # Set up for fieldlines calc
            X=np.linspace(np.min(Vx),np.max(Vx),nx,endpoint=True)
            Y=np.linspace(np.min(Vy),np.max(Vy),ny,endpoint=True)
            Z=np.linspace(np.min(Vz),np.max(Vz),nz,endpoint=True)
    
    
            f_x=RegularGridInterpolator((X,Y,Z),mesh3D_x)
            f_y=RegularGridInterpolator((X,Y,Z),mesh3D_y)
            f_z=RegularGridInterpolator((X,Y,Z),mesh3D_z)
    
            #X,Y=np.meshgrid(X,Y)
    
    
            n_grid=175
            
            ix=int(np.round(a/np.cbrt(Vol/n_grid)))
            iy=int(np.round(b/np.cbrt(Vol/n_grid)))
            iz=int(np.round(c/np.cbrt(Vol/n_grid)))
    
            if ix==0:
                ix=1
            if iy==0:
                iy=1
            if iz==0:
                iz=1
    
            #sys.exit()
            #ix,iy,iz=[8,8,3]
            N=500
            k=0.5
            x=np.linspace(0.1,0.9,ix,endpoint=True)
            y=np.linspace(0.1,0.9,iy,endpoint=True)
            z=np.linspace(0.1,0.9,iz,endpoint=True)

            gx,gy,gz=np.meshgrid(x,y,z)
            gx=gx.reshape(ix*iy*iz)
            gy=gy.reshape(ix*iy*iz)
            gz=gz.reshape(ix*iy*iz)
            
            point_grid=np.zeros((ix*iy*iz,3))
            point_grid[:,0]=gx
            point_grid[:,1]=gy
            point_grid[:,2]=gz
    
            
        
            if field:
                #for m,gpoint in enumerate(point_grid):
                #i,j,k=gpoint
                #    track_r=field_lines(np.array([i,j,k]),track_r)#map[m][0:counter[m]])
                lines,mags,n_track=field_lines(point_grid)            
                mags=np.power(mags,0.45)
                #mags=mags/np.max(mags)
    
                for k in range(2*len(point_grid)):
                    if n_track[k] > 1:
                        line_points=lines[k,0:n_track[k],:]
                        mag=mags[k,0:n_track[k]]
    
                        line_points=nc_fort.multmatmul(latt,line_points,len(line_points))
                        mag[0]=mag[1]            
                        #mag=mag/np.max(mag)
                        #mag[mag<0.3*np.max(mag)]=0#0.5*np.max(mag)
                        
                        r=0.008
                        
                        polyLine = pv.PolyData(line_points)
                        polyLine.points = line_points
                        polyLine["scalars"]=np.array(mag)
                        theCell = np.arange(0, len(line_points), dtype=np.int)
                        theCell = np.insert(theCell, 0, len(line_points))
                        polyLine.lines = theCell
                        tube = polyLine.tube(radius=r)
                        #p.add_mesh(tube,color="black", smooth_shading=True)
                        p.add_mesh(tube,show_scalar_bar=False,cmap="gist_yarg",opacity=mag,pickable=False)#,color="black")#cmap='binary',opacity=mag) 
    
    

    
        #Flatten the mesh
        if not xsffile:
            mesh_mag=mesh_mag.flatten('F')
        else :
            mesh_mag=mesh_mag.flatten('C')
    
        #if not xsffile:
        xs=np.linspace(0,1,nx,endpoint=True)
        ys=np.linspace(0,1,ny,endpoint=True)
        zs=np.linspace(0,1,nz,endpoint=True)

    
        points=np.zeros((mesh_mag.shape[0],3))
        counter=0
        
        for z in zs:
            for y in ys:
                for x in xs:
                    points[counter,:]=np.matmul(latt,[x,y,z])
                    counter+=1
        sgrid = pv.StructuredGrid()
        sgrid.points = points
        sgrid.dimensions=[nx,ny,nz]
    
        sgrid.point_arrays["values"] = mesh_mag
    
        if do_plane:
            # calculate the vectors
            if len(plane)>1:
                plane=nc_fort.multmatmul(latt,plane,3)
    
                v1=plane[0]-plane[1]
                v2=plane[0]-plane[2]
                print(v1,v2)
                norm=np.cross(v1,v2)
                if np.linalg.norm(norm)==0:
                    print("Plane vectors colinear: Exiting...")
                    sys.exit()
    
                v2=np.cross(norm,v1)
    
            
            cmap="autumn"#"plasma"
            if save or len(plane)>1:
                slice=sgrid.slice(norm,plane[0])
                val=slice.point_arrays["values"]
                
                p.add_mesh(slice,cmap=cmap,show_scalar_bar=False,clim=(np.min(val),sat*np.max(val)),pickable=False)
            
            else:
                p.add_mesh_slice(sgrid,show_scalar_bar=False,cmap=cmap,show_edges=False,implicit=False,clim=(np.min(mesh_mag),sat*np.max(mesh_mag)),pickable=False)
    
    
    
        if do_iso:
            if iso==None:
                iso=0.05*np.max([np.max(mesh_mag),abs(np.min(mesh_mag))])
            else:
                iso=np.float(iso[0])
    
            output.insert(END,"VOLUMETRIC\n")
            output.insert(END,"----------\n")
            output.insert(END,"Max: "+str(np.max(mesh_mag))+" Min: "+str(np.min(mesh_mag))+"\n")
            output.insert(END,"Isovalue: "+str(iso)+"\n")
            output.insert(END," \n") 
    
                
            if save or not widgets:
                contours = sgrid.contour([iso,-iso])
                slider_contour=p.add_mesh(contours, opacity=0.4,cmap=cm,smooth_shading=True,show_scalar_bar =False,lighting=True,name="contour",pickable=False)
            else:
                def cont(iso_val):
                    contours = sgrid.contour([iso_val,-iso_val])                                                                                                                                               
                    slider_contour=p.add_mesh(contours, opacity=0.4,cmap=cm,smooth_shading=True,show_scalar_bar =False,lighting=True,name="contour",pickable=False)
                    return
            
                p.add_slider_widget(cont,rng=(np.min(mesh_mag),np.max(mesh_mag)),value=iso,style="modern",title="Isosurface Value",pointa=(0.1,0.9),pointb=(0.3,0.9))
            
    ########################################################################################################################
    ########################################################################################################################
    ########################################################################################################################
    ########################################################################################################################
    ########################################################################################################################
    ########################################################################################################################
            
    
    
    # Add the box
    edges  = np.array([[[0.,0.,0.],[0.,0.,1.]],
                       [[0.,0.,0.],[0.,1.,0.]],
                       [[0.,0.,0.],[1.,0.,0.]],
                       [[1.,1.,1.],[1.,0.,1.]],
                       [[1.,1.,1.],[1.,1.,0.]],
                       [[1.,1.,1.],[0.,1.,1.]],
                       [[0.,1.,1.],[0.,0.,1.]],
                       [[0.,1.,1.],[0.,1.,0.]],                   
                       [[1.,1.,0.],[1.,0.,0.]],
                       [[1.,1.,0.],[0.,1.,0.]],
                       [[1.,0.,0.],[1.,0.,1.]],
                       [[0.,0.,1.],[1.,0.,1.]]])
    
    for i,main in enumerate(edges):
        for j,sub in enumerate(main):
            edges[i,j]=np.matmul(latt,sub)
    
    box = pv.Box([0,1,0,1,0,1])
    
    for i in range(0,12):
        p.add_lines(edges[i],color="black",width=1.5)
        
    
    # Do the atoms
    for i,vec in enumerate(pos):
        
        if i in hide_num:
            continue
        sphere=pv.Sphere(atom_radii[i]/(reduction*5),vec,theta_resolution=200, phi_resolution=200)
        p.add_mesh(sphere,color=atom_colours[i],specular=0.3,specular_power=30,ambient=0.2,diffuse=1,pickable=True)
    
    
    
    def pick_atom(actor,two):
        if actor!=None:
            centre=actor.center#np.matmul(np.linalg.inv(latt),actor.center)
            for i in range(len(pos)):
                if (np.isclose(pos[i],centre)).all():
                    break
            vec=np.round(np.matmul(np.linalg.inv(latt),pos[i]),4)
            string="Atom "+str(i)+":"+"  "+symb[i]+ "  ("+str(vec[0])+" , "+str(vec[1])+" , "+str(vec[2])+")\n"
            #print(string)
            output.insert(END,string)
            
    
    
    #p.enable_cell_picking(through=True,callback=pick_atom,show_message=False,use_mesh=True)
    p.enable_point_picking(callback=pick_atom,use_mesh=True,show_message=False,show_point=False)
    p.window_size = 1000, 1000
    p.store_image=True
    
    # Do the bonds
    #print("No. atoms: ",len(atoms))
    output.insert(END,"No. atoms: "+str(len(atoms))+"\n")
    if do_bonds:
        bond_length = []
        bond_name   = []
        bond_ind    = []
        bonds=np.zeros(len(atoms))
        for atom1 in range(len(pos)):
            for atom2 in range(atom1,len(pos)):
    
                dist=np.sqrt(np.sum((pos[atom1]-pos[atom2])**2))
                if dist>0 and dist<bond_cut:
                    bond_length.append(dist)
                    bond_name.append(symb[atom1]+str(atom_label[atom1])+"-"+symb[atom2]+str(atom_label[atom2]))
                    bond_ind.append([atom1,atom2])
                    bonds[atom1]+=1
                    bonds[atom2]+=1
    
        
        bond_length=np.array(bond_length)
        valence=valency[atoms-1]
    
        over=-valence+bonds
        pop=[]
        for atom in range(len(over)):
            if over[atom]<=0 :
                continue
    
            atom_loc=[]
            for n,i in enumerate(bond_ind):
                if atom==i[0] or atom==i[1]:
                    atom_loc.append(n)
    
            lengths=bond_length[atom_loc]
    
            loc=np.argsort(lengths)[-int(over[atom]):]
    
    
            for i in loc:
                pop.append(atom_loc[i])
        bond_length=list(bond_length)
        pop=np.flip(np.unique(pop))
        for pi in pop:
            bond_length.pop(pi)
            bond_name.pop(pi)
            bond_ind.pop(pi)
    
            
        for i,index in enumerate(bond_ind):
    
    
            i,j=index
            if i in hide_num or j in hide_num:
                continue
            points=np.array([pos[i],(pos[j]+pos[i])/2])
            direc=points[0]-points[1]
            mid=(points[1]+points[0])/2
            height=np.sqrt(np.sum((points[0]-points[1])**2))
            cyl=pv.Cylinder(mid,direc,np.min(atom_radii)/20,height)
    
            p.add_mesh(cyl,color=atom_colours[i],pickable=False)
            points=np.array([pos[j],(pos[j]+pos[i])/2])
            direc=points[0]-points[1]
            mid=(points[1]+points[0])/2
            height=np.sqrt(np.sum((points[0]-points[1])**2))
            cyl=pv.Cylinder(mid,direc,np.min(atom_radii)/20,height)
    
            p.add_mesh(cyl,color=atom_colours[j],specular=0.3,specular_power=30,ambient=0.2,diffuse=1,pickable=False)
            #for i in bond_name:
            #   print(i)
    
    
    
    if do_magmom:
        for i in range(len(pos)):
            if i in hide_num:
                continue
            temp_pos=pos[i]-np.array(mom_vec[i])/2
            arrow=pv.Arrow(temp_pos,mom_vec[i],tip_length=0.15, tip_radius=0.09, tip_resolution=20, shaft_radius=0.04, shaft_resolution=20, scale='auto')
            p.add_mesh(arrow,color='b',pickable=False)
    
    
    if do_init_mag:
    
        for i in range(len(pos)):
            if i in hide_num:
                continue
            temp_pos=pos[i]-np.array(1.5*init_spin[i])/2
            arrow=pv.Arrow(temp_pos,1.5*init_spin[i],tip_length=0.15, tip_radius=0.09, tip_resolution=20, shaft_radius=0.04, shaft_resolution=20, scale='auto')
            p.add_mesh(arrow,color='g',pickable=False)
    
    try:
        p.camera.zoom(z)
    except:
        output.insert(END,"Zoom not implemented in this version of PyVista.\n")
        #print("Zoom not implemented in this version of PyVista.")
    
    
###############################################################################################

    def screenshot():
        wind=p.window_size
        height=2560
        p.window_size=[height,int(height*p.window_size[1]/p.window_size[0])]
        p.save_graphic(seed+".eps")
    
        p.window_size = wind
        if do_verbose:
            output.insert(END,"Graphic saved!\n")
    p.add_key_event("s", screenshot)
    
    def perpendicular( a ) :
        b = np.empty_like(a)
        b[0] = -a[1]
        b[1] = a[0]
        return b
    
    # calculate the reciprocal lattice vectors
    a1=latt[:,0]
    a2=latt[:,1]
    a3=latt[:,2]
    b1=np.cross(a2,a3)
    b2=np.cross(a3,a1)
    b3=np.cross(a1,a2)
    focus=np.matmul(latt,np.array([0.5,0.5,0.5]))
    
    
    if orient != None:
        cp=p.camera_position
    
        if orient=='a':
            o=a3
            vpvec=a1/np.linalg.norm(a1)
        elif orient=='a*':
            o=a3
            vpvec=b1/np.linalg.norm(b1)
        elif orient=='b':
            o=a3
            vpvec=a2/np.linalg.norm(a2)
        elif orient=='b*':
            o=a3
            vpvec=b2/np.linalg.norm(b2)
        elif orient=='c':
            o=a2
            vpvec=a3/np.linalg.norm(a3)
        elif orient=='c*':
            o=a2
            vpvec=b3/np.linalg.norm(b3)
        elif orient=="sd":
            o=a3
            #T=[latt[i,i] for i in range(0,3)]
            T=0.9*a1+0.4*a2+0.1*a3
            vpvec=T/np.linalg.norm(T)
    
    
        vp=focus+15*vpvec
        p.camera_position=[vp,focus,o]
    
    if np.sum(cam_pos)!=0:
        v=(cam_pos[0],cam_pos[1],cam_pos[2])
        o=(cam_pos[3],cam_pos[4],cam_pos[5])
        p.camera_position=[v,focus,o]
    
    def button_sd():
     
        o=a3
        T=0.9*a1+0.4*a2+0.1*a3
        vpvec=T/np.linalg.norm(T)
        vp=focus+15*vpvec
        p.camera_position=[vp,focus,o]
    
            
            
    def button_a():
        o=a3
        vpvec=a1/np.linalg.norm(a1)
        
        vp=focus+15*vpvec
        p.camera_position=[vp,focus,o]
     
    
    def button_b():
    
        o=a3
        vpvec=a2/np.linalg.norm(a2)
        vp=focus+15*vpvec
        p.camera_position=[vp,focus,o]
    
    
    def button_c():
    
        o=a2
        vpvec=a3/np.linalg.norm(a3)
        
        vp=focus+15*vpvec
        p.camera_position=[vp,focus,o]
    
    
            
    end=time.perf_counter()
    p.add_key_event("o",button_sd)
    p.add_key_event("a",button_a)
    p.add_key_event("b",button_b)
    p.add_key_event("c",button_c)
    
    
    
    

        
    output.insert(END,"Time: "+str(np.round(end-start,4))+" s\n")
    if save:
        p.window_size=[5000,5000]
        #p.save_graphic(seed+".pdf")
        p.show(title=seed,screenshot=seed+".png")
    else:
    
        p.show(title=seed)
    
    if do_verbose:
        print("Final Camera Position:")
        print(p.camera_position[0][0],p.camera_position[0][1],p.camera_position[0][2],p.camera_position[2][0],p.camera_position[2][1],p.camera_position[2][2])
    
    sys.exit()
    
    
    window.mainloop()


if __name__== "__main__":
    main()
