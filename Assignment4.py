"""

Assignment 4: Creating dash app in Python
Name: Habib Khan

"""

# Importing libraries

import pandas as pd
import numpy as np
import dash
from dash.dependencies import Input, Output
import dash_core_components as dcc
import dash_html_components as html
from pandas.api.types import CategoricalDtype
import matplotlib.pyplot as plt

import datashader as ds
import datashader.transfer_functions as tf
import datashader.glyphs
from datashader import reductions
from datashader.core import bypixel
from datashader.utils import lnglat_to_meters as webm, export_image
from datashader.colors import colormap_select, Greys9, viridis, inferno
from functools import partial

""" Now let's download the file from data source given """

link = 'trees.csv'
file = pd.read_csv(link)
file.head(10)


""" Data Cleaning """

cattype = CategoricalDtype(categories=["Poor", "Fair", "Good"], ordered= True) # Converting into categories and ordered
file['health']= file['health'].astype(cattype)
print(file['health'].describe())
print(file['health'].cat.codes[:10]) 

file['health'].isna().sum() # Checking the missing values
file['health'] = file['health'].fillna('Fair') # Replacing the missing values with "Fair" to avoid biases
file['health'] = file['health'].cat.codes
file['steward']= file['steward'].fillna('None')

# Creating histogram to see distribution
file['health'].hist()
plt.show()


# Creating a Dash App

app = dash.Dash() 

app.layout = html.Div(children=[
    html.H1(children = 'Health of the trees in NYC'),
    html.P('Select a Borough to view the result: '),
    dcc.RadioItems(
        id='dropdown-1',
        options=[{'label': i, 'value': i} for i in ['Bronx', 'Brooklyn', 'Manhattan', 'Queens', 'Staten Island']],
        value='Queens'
    ),
    html.Div(id='output-1'),
    html.P("0 = Bad Health, 1 = Average Health, 2 = Good Health"),
    dcc.RadioItems(
        id='dropdown-2',
        options=[{'label': i, 'value': i} for i in file['steward'].unique()],
        value='None'),
    html.Div(id='output-2'),
    html.P("0 = Bad Health, 1 = Average Health, 2 = Good Health")

    ])


# Creating call back

@app.callback(
    Output(component_id='output-1', component_property='children'),
    [Input(component_id='dropdown-1', component_property='value')]
    )


# Creating graphs

def graph(input_data):
    df = file[file.boroname == input_data]

    return dcc.Graph(
        id='Health of Trees by boroughs',
        figure={
            'data':[
                {'x':df['health'], 'type': 'histogram', 'name': 'Health of Trees by boroughs'}
                ],
            'layout':{
                'title': "Health of Trees by boroughs"}})


# Working on Q2

# Create callback

@app.callback(
    Output(component_id='output-2', component_property='children'),
    [Input(component_id='dropdown-2', component_property='value')]
    )


def steward(input_data):
    df= file[file.steward == input_data]

    return dcc.Graph(
        id= 'Health of Trees by Steward',
        figure= {
            'data':[
                {'x':df['health'], 'type': 'histogram','name': 'Health of Trees by Steward',}],
            'layout':{
                'title': "Health of Trees by Steward"}})

# Creating a server

if __name__ == '__main__':
    app.run_server(debug=False)



