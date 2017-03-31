var colorScales = {
  "BuYl": {
    "png": "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAASwAAAAKCAYAAAAO/2PqAAAABHNCSVQICAgIfAhkiAAAAAlwSFlzAAAN1wAADdcBQiibeAAAABl0RVh0U29mdHdhcmUAd3d3Lmlua3NjYXBlLm9yZ5vuPBoAAADbSURBVGiB7dRNTsMwFEXhcw3bgKr7YSEdMGUjDLoT1sMAlWXETCJ+qiat20jB0vkm0R3Y8fNLXh5f3p5KGV4J21ABCJUEoBLG53EGSGP+3md8x017NGQg+amNP3nB+sb8+x5JS768vtk9Z+udyKfOclzP2TXtef5e2+pb/Lu5sjdT39GSvWlfc2svpvLp3iz5T4cKpb7fwfM9YQ88IEn/Vc12oO5LUh1WknqwKWufQJIu5cCS1A0HlqRuOLAkdcOBJakbBTisfQhJOit8lDpkB3yufRZJmnEosPsCev2iBz+I46kAAAAASUVORK5CYII=",
    "colors": [
      "#1f77b4",
      "#6d9a6b",
      "#bcbd22"
    ]
  },
  "YlBu": {
    "png": "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAASwAAAAKCAYAAAAO/2PqAAAABHNCSVQICAgIfAhkiAAAAAlwSFlzAAAN1wAADdcBQiibeAAAABl0RVh0U29mdHdhcmUAd3d3Lmlua3NjYXBlLm9yZ5vuPBoAAADgSURBVGiB7dRBbgIxDEDR75RrVBU9Tw/CoqfpgtNwFxYVPQbuBlApEzJpQcNI/20iL+I4TuTYbF7fsvDBPpZJkAAEmYe1FgPk9TgzAEh+rElXjmN8maMjHqrldE7fnlo8WPuYPp7Fh1xde8a9TfMuFz2401s0+nx+7m3eppbnL2/Te7+eP16Nh2qp/ufb/pvmH/hHjlZtv/q6zcL7gsKajGck6XEtyVgXEoeVpMeXvJSpa5CksRxYkmbDgSVpNhxYkmbDgSVpNgqwm7oISWrJjM9CsgK+pi5Gkq7YxdN+9Q1dBfS0EQdkVwAAAABJRU5ErkJggg==",
    "colors": [
      "#bcbd22",
      "#6d9a6b",
      "#1f77b4"
    ]
  },
  "RdBu": {
    "png": "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAASwAAAAKCAYAAAAO/2PqAAAABHNCSVQICAgIfAhkiAAAAAlwSFlzAAAN1wAADdcBQiibeAAAABl0RVh0U29mdHdhcmUAd3d3Lmlua3NjYXBlLm9yZ5vuPBoAAAD8SURBVGiB7dRNTsMwEIbh7zNcA6HuehgO0gWnYdGrsewClWN0WOSHJHXt0oKCpffZVLI9nRknGb9vty+n8FtYG8kKSWFLkkL9r7v1YV/u9sY1z88Osd3Z4Vy/5sn/ZGLnefp6JmfHGuWxjlzsdx2ZPJPYeZ5lD5k8yx7OYgt5qj0U8lR7uBxb76HwPGs9VGrM719xF38c+/Nn9AuxWnxbxXtX4b2svRv3xRa//+K93xJ74W7OYw+R9PoYEXvJTwKA/2uj8D4xrAA0IfSc1q4BAK7FwALQDAYWgGYwsAA0g4EFoBlJ0nHtIgCgJsIfydZO0ufaxQBAwdEPp90XBfvJteOpLAgAAAAASUVORK5CYII=",
    "colors": [
      "#d62728",
      "#7a4f6e",
      "#1f77b4"
    ]
  },
  "BuRd": {
    "png": "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAASwAAAAKCAYAAAAO/2PqAAAABHNCSVQICAgIfAhkiAAAAAlwSFlzAAAN1wAADdcBQiibeAAAABl0RVh0U29mdHdhcmUAd3d3Lmlua3NjYXBlLm9yZ5vuPBoAAADzSURBVGiB7dRNTsNADEDh5xHXANT7cJAu2HIRFr0J52GByjFqFm1DmDQ/UFVhpPdtIo3HkT2ZOB5e3p5KObxGsoEkgMgEIDg987h+jpPHWLeW37Hx3NPaFbn9vefc/t5uH8CNcvs9/Pps6ty6rityJ/ubPffb5V6O/6G/Lr68xmF8wTcY7WHBWYy++9Ldmuth/F7O9zC8lz/+rckeqhrrOibPqqpxInfwPesa6x6S90ie7wh2wD2S9H9tiNyViHRYSWrBY1m7AklayoElqRkOLEnNcGBJaoYDS1IzCrBfuwhJmpcfJQ+xBT7XLkWSJuzJsv0C0xnrjgY9PbUAAAAASUVORK5CYII=",
    "colors": [
      "#1f77b4",
      "#7a4f6e",
      "#d62728"
    ]
  },
  "RdBk": {
    "png": "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAASwAAAAKCAYAAAAO/2PqAAAABHNCSVQICAgIfAhkiAAAAAlwSFlzAAAN1wAADdcBQiibeAAAABl0RVh0U29mdHdhcmUAd3d3Lmlua3NjYXBlLm9yZ5vuPBoAAADeSURBVGiB7dTBqQIxGEXhO8E25OHOYizEhdW8hY1ZgAvRMrwujJK8ZOLownmB8zEDEw4kPwoZDuv1Jli/llaWFZ/nq+zbsj9pkpNetnsfPzue0Do76dUWe3Wux7rY3/m62N/52o1W7J/MVezfapW5srMrLTu7Mtdz/5GW9GqTZI83xf5RS/rb7cVczRZ/lHdbjzP//R+mti/PdbS0WwR7Lw1LAcD/tRqkfeCyAtCJnzD3BAAwFRcWgG5wYQHoBhcWgG5wYQHoRpB0nnsIAJjgFK6DtpIuc08CAA1nS9sbiMQxeZdo9AkAAAAASUVORK5CYII=",
    "colors": [
      "#d62728",
      "#6b1414",
      "#000000"
    ]
  },
  "BkRd": {
    "png": "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAASwAAAAKCAYAAAAO/2PqAAAABHNCSVQICAgIfAhkiAAAAAlwSFlzAAAN1wAADdcBQiibeAAAABl0RVh0U29mdHdhcmUAd3d3Lmlua3NjYXBlLm9yZ5vuPBoAAADcSURBVGiB7dRBCsIwFIThmeA1RHofD+LC07jo9VyIXiNugqZNkxY3NfB/4KKO5A2pPEs6W7pZGmxLkpx98mfZv2WSnOVFlvLq7L16bej8U9Zp5/l72Jqt97LsRueUV7P0sNgrfbN0/idL+TTT5xfNrDo7u8ti9uyei/Nnd+lGlp1f9MrOr2aT+eX7/85v/W8qvSbnt2ZXsunsu6OvB0ujpKMA4H8N0XEMYlkB6MMp7N0AALZiYQHoBgsLQDdYWAC6wcIC0I0g6bl3CQBYFx8hShdJr72rAEDD0zFc3qwdD46zrh34AAAAAElFTkSuQmCC",
    "colors": [
      "#000000",
      "#6b1414",
      "#d62728"
    ]
  },
  "RdBkGr": {
    "png": "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAASwAAAAKCAYAAAAO/2PqAAAABHNCSVQICAgIfAhkiAAAAAlwSFlzAAAN1wAADdcBQiibeAAAABl0RVh0U29mdHdhcmUAd3d3Lmlua3NjYXBlLm9yZ5vuPBoAAAECSURBVGiB7dSxbcMwEIXh/24PI1DnYTKIi0yTwotlgBSBM4aeC0cOLVMy5RSKgPc10oHUuwNtMD72+1fEu6ROQA9IujyL+voO9KP6dr+Qim8R/aiWprJGa6N6PutnhqK+5Ldk3e6tZQ31/f5ifv096/dMi7Xr/uKcNc6aWJvIGura7znUtf/DfVaxVn4bQEBEQOV9bi0iIB/vm1zL9l7P9p7MWNC7ui+XndVc78UZld7NGU/2Lmd/kP+Z5FumdAQ6zMz+K9EJHRNit/YsZmYNXnLtCczMWvnCMrPN8IVlZpvhC8vMNsMXlpltRgKntYcwM2vwlX1wEHyvPYmZ2YxTEIcz9Cz1Yl1UT7gAAAAASUVORK5CYII=",
    "colors": [
      "#d62728",
      "#000000",
      "#2ca02c"
    ]
  },
  "GrBkRd": {
    "png": "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAASwAAAAKCAYAAAAO/2PqAAAABHNCSVQICAgIfAhkiAAAAAlwSFlzAAAN1wAADdcBQiibeAAAABl0RVh0U29mdHdhcmUAd3d3Lmlua3NjYXBlLm9yZ5vuPBoAAAEBSURBVGiB7dQxUsMwEIXh/wmuwTDpcxQOkoLTUKTkahRMOIaXwiZxFEWWoTCaeV8TaVZ+u+N4pP37/iUUbwzsCCAgIrhZD5Vati7Whkqt9tzUu+ncQu9fZazoXTy3svfVuaEhf2Xv5oxK75YMBQhIEmJaA5LGXyAhpHJt/myilDU+ezcLkbL95XyttpQ1zTDPyvbjrOU5/5x13s/ejfJaOetnf14vZRXP12rX+9us8v95OT//NrLvQfp4CL0+Io4ET5iZ/V+7UBwT+LIysy48p60nMDNr5QvLzLrhC8vMuuELy8y64QvLzLqRgNPWQ5iZLYvPRHAAvrYexcys4qRIh2/yRf2NvFaugwAAAABJRU5ErkJggg==",
    "colors": [
      "#2ca02c",
      "#000000",
      "#d62728"
    ]
  },
  "BkWh": {
    "png": "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAASwAAAAKCAYAAAAO/2PqAAAABHNCSVQICAgIfAhkiAAAAAlwSFlzAAAN1wAADdcBQiibeAAAABl0RVh0U29mdHdhcmUAd3d3Lmlua3NjYXBlLm9yZ5vuPBoAAACNSURBVGiB7dTNCcJAEIbhdwbbEEk/FpKD1XhIex4kaWNsQCQGYXfxfWAvO8zP6Qvgmpl3YIoI3j3gq/+jtV/Pc1d/u/6lp/X+nnsOznpk5u0UEQtwRpL6NVXVkhhWksZwydYXSNJeBpakYRhYkoZhYEkahoElaRgJrK2PkKQdnllVM7C1vkSSPlgjYn4B1s8RZj5J2KAAAAAASUVORK5CYII=",
    "colors": [
      "#000000",
      "#808080",
      "#ffffff"
    ]
  },
  "WhBk": {
    "png": "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAASwAAAAKCAYAAAAO/2PqAAAABHNCSVQICAgIfAhkiAAAAAlwSFlzAAAN1wAADdcBQiibeAAAABl0RVh0U29mdHdhcmUAd3d3Lmlua3NjYXBlLm9yZ5vuPBoAAACMSURBVGiB7dS7DcJAEITh2RNtIOR+KMSBqyFwewTIlLFDDpIf0XnR/0mX7Gm0G03YvmfmQ9JgW99P0s9s6+/onEz//VUz7PqfXRuZZ2ZOF9uzpKsA4LyGiJibKCsANdxa7wsAYC8KC0AZFBaAMigsAGVQWADKaJKW3kcAwA6vFhGjpHfvSwBgxWJ7/ADQIgWWAOHYEwAAAABJRU5ErkJggg==",
    "colors": [
      "#ffffff",
      "#808080",
      "#000000"
    ]
  }
}
