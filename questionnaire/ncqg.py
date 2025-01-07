import matplotlib.pyplot as plt
import matplotlib.patches as patches

# Create figure and axis
fig, ax = plt.subplots(figsize=(10, 6))

translate = 200

# Add the background rectangle
gdp_rect = patches.Rectangle((translate, 0), 200, 350, linewidth=1, edgecolor='black', facecolor='gray', zorder=1)
ax.add_patch(gdp_rect)
ax.text(100+translate, 250, '$70,000 billion: GDP of developed countries', color='black',
        fontsize=10, ha='center', va='center', zorder=2)

# Define rectangle properties
rectangles = [
    {'xy': (translate, 0), 'width': 50, 'height': 100, 'color': 'yellow', 'legend': '$5,000 billion\nThe goal called for by Demand Climate Justice\n(a network of NGOs including 350.org and\nthe World Council of Churches)'},
    {'xy': (translate, 0), 'width': 20, 'height': 50, 'color': 'orange', 'legend': '$1,000 billion\nThe goal called for by Climate Action Network\n(a network of NGOs including Greenpeace, Oxfam, and WWF)'},
    {'xy': (translate, 0), 'width': 15, 'height': 40, 'color': 'orangered', 'legend': '$600 billion\nThe goal called for by India,\na position shared by most developing countries'},
    {'xy': (translate, 0), 'width': 12, 'height': 25, 'color': 'red', 'legend': '$300 billion\nThe new goal, if all climate finance were provided as grants'},
    {'xy': (translate, 0), 'width': 10, 'height': 10, 'color': 'darkred', 'legend': '$100 billion\nThe old goal, if all climate finance were provided as grants'},
    {'xy': (translate, 0), 'width': 5, 'height': 5, 'color': 'black', 'legend': '$26 billion\nThe current amount, consistent with the old goal'},
]

def offset(y):
  if y == 5: 
    z = -20
  elif y == 10:
    z = 10
  elif y == 25:
    z = 35
  elif y == 40:
    z = 60
  elif y == 50:
    z = 90
  elif y == 100:
    z = 110
  else:
    z = y
  return z
    

# Add the rectangles and their legends
for rect in rectangles:
    r = patches.Rectangle(rect['xy'], rect['width'], rect['height'],
                          linewidth=0.5, edgecolor='black', facecolor=rect['color'], zorder=3)
    ax.add_patch(r)
    
    # Add arrow and text for each rectangle
    arrow_x = rect['xy'][0] - 1
    arrow_y = rect['xy'][1] + rect['height'] * 0.9
    if rect['height'] == 40:
      arrow_y = 32
    if rect['height'] == 25:
      arrow_y = 17
    if rect['height'] == 10:
      arrow_y = 7
    if rect['height'] == 5:
      arrow_y = 2
    ax.annotate(rect['legend'], xy=(arrow_x, arrow_y), xytext=(-110-translate, offset(rect['height'])),
                textcoords='offset points', arrowprops=dict(arrowstyle='->', color=rect['color']),
                fontsize=10, color='black', va='center')

# Set axis limits and hide axes
ax.set_xlim(0, 401)
ax.set_ylim(-1, 351)
ax.axis('off')

# Show the plot
plt.tight_layout()
plt.savefig("C:/Users/fabre/Documents/www/robustness_global_redistr/questionnaire/ncqg_EN.svg")
plt.show()
