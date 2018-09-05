import matplotlib.pyplot as plt

x = [50, 60, 70, 100, 150, 300, 400, 500, 600, 700, 800, 900, 1200, 1500, 3000, 5000, 7000, 9000, 12000]
y = [0, 0.013, 0.015, 0.0155, 0.031, 0.046, 0.0625, 0.0666, 0.1, 0.171, 0.2, 0.24, 0.39, 0.6, 1.25, 2.89, 5.03, 7.83, 12.71]

plt.plot(x, y)
plt.title('Quadratic performance of ppexpr')
plt.show()