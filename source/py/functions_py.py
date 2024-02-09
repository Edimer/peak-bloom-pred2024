# Libraries
from owslib.wcs import WebCoverageService

# This function allows extracting data from soilgids and exports a .tif file
def extractSoilGrids(variables, limits, res, pathExp, country):
    for i in range(len(variables)):
        variable_i = i
        print(f"Starting process with {variables[variable_i]} ======")
        url = "http://maps.isric.org/mapserv?map=/map/{}.map".format(
            variables[variable_i])
        wcs = WebCoverageService(url, version="1.0.0")

        if variables[variable_i] != "ocs":
            layer_prom = [k for k in wcs.contents.keys() if k.startswith(
                "{}_0-5cm_mean".format(variables[variable_i]))][0]
        else:
            layer_prom = [k for k in wcs.contents.keys() if k.startswith(
                "{}_0-30cm_mean".format(variables[variable_i]))][0]
        limits_box = limits
        crs = 'urn:ogc:def:crs:EPSG::4326'
        resolution = res
        format_export = wcs.contents[layer_prom].supportedFormats[0]

        result = wcs.getCoverage(
            identifier=layer_prom,
            crs=crs,
            bbox=limits_box,
            resx=resolution,
            resy=resolution,
            format=format_export)

        name_export = f"{pathExp}{layer_prom}_{country}.tif"
        with open(name_export, 'wb') as file:
            file.write(result.read())

        print(f"Process finished with {variables[variable_i]}! ======")