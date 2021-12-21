# ----- this tool can be run from the command line -----
# open Start Menu > ArcGIS > Python Command Prompt
# run this: C:\ArcGIS\Pro\bin\Python\scripts\propy.bat C:\Users\b35143921880\Documents\mergeFCs_V3_6AM_8AM_2PM_4PM.py
# change file path of the mergeFCs.py in the code above

import os

#cidades = ["bel", "bho", "bsb", "cam","cgr","cur","duq","for_","goi","gua","mac","man","nat","poa","rec","rio","sal","sgo","slz"]
cidades = ["bel", "bho","cam","cur","duq","for_","goi","gua","mac","nat","poa","rec","sal","sgo","slz"]
hora = ["06AM","0615AM","0630AM","0645AM","07AM","0715AM","0730AM","0745AM","08AM","02PM","0215PM","0230PM","0245PM","03PM","0315PM","0330PM","0345PM","04PM"]


for k in range(0,len(cidades),1):
    for j in range(0,len(hora),1):

        #Setting root workspace where GDB files are located  
        base_dir = r'C:\Users\b35143921880\Documents\Processamentos\matriz_TI'
        city = cidades[k]
        horario = r'wkday' + hora[j]
        travel = r'travel_time_workers'
        
        workspace = os.path.join(base_dir,city,horario,travel)
        #r"C:\Users\b35143921880\Documents\wkday16h\travel_time_workers"

        # set output folder name
        output_folder = os.path.join(base_dir,city,horario)
        #r"C:\Users\b35143921880\Documents\wkday16h"

        # output file name
        output_file_name = hora[j] + ".csv.gzip"

        def arcgis_table_to_df(in_fc, input_fields=None, query=""):
            """Function will convert an arcgis table into a pandas dataframe with an object ID index, and the selected
            input fields using an arcpy.da.SearchCursor.
            :param - in_fc - input feature class or table to convert
            :param - input_fields - fields to input to a da search cursor for retrieval
            :param - query - sql query to grab appropriate values
            :returns - pandas.DataFrame"""
            OIDFieldName = arcpy.Describe(in_fc).OIDFieldName
            if input_fields:
                final_fields = [OIDFieldName] + input_fields
            else:
                final_fields = [field.name for field in arcpy.ListFields(in_fc)]
            data = [row for row in arcpy.da.SearchCursor(in_fc,final_fields,where_clause=query)]
            fc_dataframe = pd.DataFrame(data,columns=final_fields)
            fc_dataframe = fc_dataframe.set_index(OIDFieldName,drop=True)
            return fc_dataframe


        #Importing Modules  
        import arcpy  
        import os
        import pandas as pd

        print("meow")



        #Making an empty list to put filepaths to csv into
        dfs = []
        i = True
        #Walking workspace recursively checking type, and appending filepath to list  
        for dirpath, dirnames, filenames in arcpy.da.Walk(workspace,datatype="FeatureClass"):  
            for filename in filenames:
                fc = os.path.join(dirpath, filename)

                print(fc)
                # feature_classes.append()

                # convert to dataframe
                df = arcgis_table_to_df(fc, ["Total_Time","OriginName","DestinationName"])

                # round to save space
                df["Total_Time"] = df["Total_Time"].round(1)
                # df["Total_Distance"] = df["Total_Distance"].round(1)

                # subset to only include travel time of less than 2 hours (120 min)
                df = df[df["Total_Time"] < 120]

                if i == True:
                    df.to_csv(os.path.join(output_folder, output_file_name), index = False, compression = "gzip")
                    i = False

                else:
                    df.to_csv(os.path.join(output_folder, output_file_name), index = False, compression = "gzip", mode='a', header=False)



