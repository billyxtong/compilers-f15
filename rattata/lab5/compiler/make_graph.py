import csv
import sys
import numpy as np
import matplotlib.pyplot as plt
import matplotlib
import os

def getTestFileNames(main_dir, csvfile):
    # we can just use the first one
    with open(os.path.join(main_dir, csvfile)) as f:
        reader = csv.reader(f)
        names = []
        for row in reader:
            names.append(row[0])
    return names

def getTimesForFile(csvfile):
    result = []
    with open(csvfile) as f:
        reader = csv.reader(f)
        for row in reader:
            result.append(int(row[1]))
    return result

def getLegendForFile(filename):
    filename = filename[:filename.find('.')]
    flags_section = filename.split('_')[1]
    return ' '.join(flags_section.split('-'))

def make_plot(main_dir, filesToUse):
    times_for_O0 = getTimesForFile(os.path.join(main_dir, 'bench0_O0.csv'))
    
    # some stuff from http://matplotlib.org/examples/api/barchart_demo.html
    barWidth = .2
    testFileNames = getTestFileNames(main_dir, filesToUse[0]) # just use the
    # first one, they should all have the same test file names
    x_vals = np.arange(len(testFileNames))
    (fig, ax) = plt.subplots()
    plotted_bars = []
    colors = ['r', 'b', 'y']
    for i in xrange(len(filesToUse)):
        filename = filesToUse[i]
        filepath = os.path.join(main_dir, filename)
        times_for_file = getTimesForFile(filepath)
        calibrated_times = [float(times_for_file[j])/times_for_O0[j]
                            for j in xrange(len(times_for_file))]
        filename = filesToUse[i]
        new_bar = ax.bar(x_vals + barWidth * i, calibrated_times, barWidth, 
                         color = colors[i])
        plotted_bars.append(new_bar)

    ax.set_xticks(x_vals + barWidth)
    smallFontsize = 10
    ax.set_xticklabels(testFileNames, fontsize = smallFontsize)
    legend = [getLegendForFile(f) for f in filesToUse]
    plt.legend(legend)
    largeFontsize = 20
    plt.ylabel("Cycle Count / O0 Cycle Count", fontsize = largeFontsize)
    plt.show()
                        
def main():
    assert(len(sys.argv) == 2)
    main_dir = sys.argv[1]
    csvFiles = [f for f in os.listdir(main_dir) if f.endswith(".csv")]
    filesToUse = csvFiles[:3]
    filesToUse = ['bench0_O0-unsafe.csv', 'bench0_O1-unsafe.csv',
                  'bench0_O2-unsafe.csv']
    make_plot(main_dir, filesToUse)

if __name__ == "__main__":
    main()
