import numpy as np
import matplotlib.pyplot as plt
import sys

def main():
    INPUT = "/home/trygveasp/tmp/build/fc2018010318+003grib_fp_mbr000"
    OUTPUT = '/home/trygveasp/tmp/build/out.set.grib'

    try:
        from eccodes import codes_grib_new_from_file,codes_get,codes_get_size,codes_write,codes_release,codes_get_values
    except:
        print "Missing eccodes"
        sys.exit(1)

    fin = open(INPUT)
    fout = open(OUTPUT, 'w')

    keys = [
        'Ni',
        'Nj',
        'latitudeOfFirstGridPointInDegrees',
        'longitudeOfFirstGridPointInDegrees',
        'indicatorOfParameter',
        'level',
        'timeRangeIndicator',
        'indicatorOfTypeOfLevel',
        'dataDate',
        'dataTime'

    ]
    geography=["bitmapPresent",
               "Nx",
               "Ny",
               "latitudeOfFirstGridPointInDegrees",
               "longitudeOfFirstGridPointInDegrees",
               "LoVInDegrees",
               "DxInMetres",
               "DyInMetres",
               "iScansNegatively",
               "jScansPositively",
               "jPointsAreConsecutive",
               "Latin1InDegrees",
               "LaDInDegrees",
               "Latin2InDegrees",
               "latitudeOfSouthernPoleInDegrees",
               "longitudeOfSouthernPoleInDegrees",
               "gridType"
    ]

    while 1:
        gid = codes_grib_new_from_file(fin)
        if gid is None:
            print "Not found"
            break
        else:
            w_par=6
            w_lev=0
            w_typ="sfc"
            w_tri=0
            par=codes_get(gid,"indicatorOfParameter")
            lev=codes_get(gid,"level")
            typ=codes_get(gid,"indicatorOfTypeOfLevel")
            tri=codes_get(gid,"timeRangeIndicator")
            if w_par == par and w_lev == lev and w_typ == typ and w_tri == tri:
                print "Found:",par,lev,typ,tri

                geo={}
                for key in geography:
                    try:
                        geo.update({key:codes_get(gid, key)})
                    except CodesInternalError as err:
                        print('Error with key="%s" : %s' % (key, err.msg))

                for key in geo:
                    print('  %s: %s' % (key, geo[key]))

                print('There are %d values, average is %f, min is %f, max is %f' % (
                    codes_get_size(gid, 'values'),
                    codes_get(gid, 'average'),
                    codes_get(gid, 'min'),
                    codes_get(gid, 'max')
                ))

                values = codes_get_values(gid)
                nx=geo["Nx"]
                ny=geo["Ny"]
                print('%d values found in %s' % (len(values), INPUT))
                field=np.empty([nx,ny])
                ii=0
                X=np.arange(0,nx,1)
                Y=np.arange(0,ny,1)
                for j in range(0,ny):
                    for i in range(0,nx):

                        #print i,j,ii
                        field[i,j]=values[ii]
                        ii=ii+1


                print "Trygve",values.shape,field.shape
                field2=field[300:400,300:450]
                plt.contourf(np.transpose(field2))
                plt.show()
                break


        #codes_set_key_vals.
        codes_write(gid, fout)
        codes_release(gid)
    fin.close()
    fout.close()


if __name__ == "__main__":
    sys.exit(main())
