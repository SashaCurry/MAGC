import matplotlib.pyplot as plt

def read_coordinates(filename):
    coordinates = []
    with open(filename, "r") as file:
        for line in file:
            x, y = map(int, line.strip().split(' '))
            coordinates.append((x, y))
    print(coordinates)
    return coordinates

def plot_coordinates(coordinates):
    x_values, y_values = zip(*coordinates)
    plt.scatter(x_values, y_values, s=15)
    plt.title('Coordinates Graph')
    plt.xlabel('X')
    plt.ylabel('Y')
    plt.grid()
    plt.show()

def plot_coordinates_alt(coordinates):
    x_values, y_values = zip(*coordinates)
    plt.scatter(x_values, y_values, s=25)
    plt.title('Coordinates Graph')
    plt.xlabel('X')
    plt.ylabel('Y')
    plt.grid()

    for i, (x, y) in enumerate(coordinates):
        plt.annotate(f"({x}, {y})", (x, y), textcoords="offset points", xytext=(0, 10), ha='center', fontsize=8)

    plt.show()

coordinates = read_coordinates('points.txt')
plot_coordinates(coordinates)
# plot_coordinates_alt(coordinates)
