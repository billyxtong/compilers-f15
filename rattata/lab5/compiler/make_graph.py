import csv
import sys
import numpy as np
import matplotlib.pyplot as plt
import matplotlib
import os

def getTestFilesNames(main_dir, csvFiles):
    # we can just use the first one
    with open(os.path.join(main_dir, csvFiles[0])) as f:
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

def make_plot(main_dir, filesToUse):
    all_times = []
    for filename in filesToUse:
        filepath = os.path.join(main_dir, filename)
        all_times.append(getTimesForFile(filepath))

    # some stuff from http://matplotlib.org/examples/api/barchart_demo.html
    barWidth = .2
    x_vals = np.arange(len(all_times[0])) # number of test files
    (fig, ax) = plt.subplots()
    plotted_bars = []
    colors = ['r', 'b', 'y']
    for i in xrange(len(filesToUse)):
        filename = filesToUse[i]
        times_for_file = all_times[i]
        new_bar = ax.bar(x_vals + barWidth * i, times_for_file, barWidth, 
                         color = colors[i])
        plotted_bars.append(new_bar)

    fontsize = 20
    plt.legend(filesToUse)
    plt.ylabel("Cycle Count", fontsize = fontsize)
    plt.show()
                        
def main():
    assert(len(sys.argv) == 2)
    main_dir = sys.argv[1]
    csvFiles = [f for f in os.listdir(main_dir) if f.endswith(".csv")]
    filesToUse = csvFiles[:3]
    filesToUse = ['bench0FormattedOutput_O0.csv', 'bench0FormattedOutput_ConstOpts.csv']
    make_plot(main_dir, filesToUse)

if __name__ == "__main__":
    main()
