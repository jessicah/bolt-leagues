from bokeh.core.properties import value
from bokeh.io import show, output_file, save, export_png, export_svgs
from bokeh.models import ColumnDataSource, Label, FactorRange, CategoricalColorMapper
from bokeh.plotting import figure
from bokeh.transform import stack

output_file("c:/users/jessi/documents/bokehbokeh.html")

num_weeks = 8

weeks = [i+1 for i in range(num_weeks)]
cats = ["A", "B", "C", "D"]

factors = [("Week {}".format(week), "Cat {}".format(cat)) for week in weeks for cat in cats]

classification = ["existing", "new"]

existing = [
    [  9, 20, 28, 10],
    [  5, 11, 15,  4 ],
    [  5, 16, 18,  4 ],
    [  8, 15, 18,  4 ],
    [ 11, 12, 20,  3 ],
    [ 11, 22, 17,  5 ],
    [  7, 16, 23,  7 ],
    [ 10, 17, 23,  8 ],
]
new = [
    [  0,  0,  0,  0 ],
    [  7, 11, 20,  3 ],
    [  4,  9,  6,  4 ],
    [  5,  6,  0,  3 ],
    [  5,  4,  4,  3 ],
    [  1,  7, 17,  4 ],
    [  9,  7, 10,  3 ],
    [  6,  8, 12,  4 ],
]

total = [ sum(e) + sum(n) for (e,n) in zip (existing, new) ]

existing_flat = [ item for sublist in existing for item in sublist ]
new_flat = [ item for sublist in new for item in sublist ]
totals = [ a+b for (a,b) in zip(existing_flat, new_flat) ]

max_heights = [max(totals[i:i+4]) for i in range(0, len(totals), 4)]

source = ColumnDataSource(data = dict(
    x = factors,
    existing = existing_flat,
    new = new_flat,
    cats = ['a', 'b', 'c', 'd'] * num_weeks,
))
color_mapper1 = CategoricalColorMapper(factors=['a', 'b', 'c', 'd'], palette=['#ffc000','#ffc000','#ffc000','#ffc000'])
color_mapper2 = CategoricalColorMapper(factors=['a', 'b', 'c', 'd'], palette=['#001122',"#112233","#223344","#334455"])
# newer scheme
color_mapper1 = CategoricalColorMapper(factors=['a', 'b', 'c', 'd'], palette=['#df5327','#a6b727','#418ab3','#fec306'])
color_mapper2 = CategoricalColorMapper(factors=['a', 'b', 'c', 'd'], palette=['#f2bba8',"#e1eb9e","#b1d1e2","#ffe699"])


p = figure(hidpi=True, sizing_mode="scale_width", x_range=FactorRange(*factors), plot_height=250, plot_width=900, toolbar_location=None, tools="")

#p.vbar_stack(classification, x='x', width=0.8, alpha=1, line_width=1, line_color="#ed7d31", fill_color=["#ffc000", "#ffd966"], source=source, legend=[value(x) for x in classification])
p.vbar(bottom=stack(), top=existing_flat, x=factors, width=0.8, line_color='#333333', fill_color=['#df5327','#a6b727','#418ab3','#fec306']*num_weeks)

p.vbar(bottom=existing_flat, top=totals, x=factors, width=0.8, line_color='#333333', fill_color=['#f2bba8',"#e1eb9e","#b1d1e2","#ffe699"]*num_weeks)

#fill_color=["#ffc000", "#ffd966"]
p.y_range.start = 0
p.y_range.end = 40
p.x_range.range_padding = 0.02
#p.x_range.group_padding = 10
p.xaxis.major_label_orientation = 1
p.yaxis.minor_tick_line_color = None
p.xgrid.grid_line_color = None
p.legend.location = "top_center"
p.legend.orientation = "horizontal"

#labels = LabelSet(x='x', y=40, text=total, level='glyph')
labels = [Label(text_font_size="8pt", text="{}".format(t), y=h+2, x=(2+5.4*i), text_align="center") for (t,i,h) in zip(total,range(len(total)),max_heights)]
#label = Label(text="67", y=35, x=(7.5+5.5), text_align="center")

p.ellipse(line_color='#ffffff', color='#fcfcfc', width=1.1, height=4, x=[2+5.4*i for i in range(len(total))], y=[h+3.3 for h in max_heights])
#p.add_layout(label)
[ p.add_layout(label) for label in labels]

save(p)
#export_png(p, filename="c:/users/jessi/documents/bokehbokeh.png")
