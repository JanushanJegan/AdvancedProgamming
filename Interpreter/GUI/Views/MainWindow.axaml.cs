using Avalonia.Controls;
using System;
using Avalonia.Input;
using Microsoft.FSharp.Collections;
using OxyPlot;
using OxyPlot.Series;
using InterpreterModule;
namespace GUI.Views
{
    public partial class MainWindow : Window
    {
        private FSharpMap<string, Interpreter.Value> storedVariables = MapModule.Empty<string, Interpreter.Value>();
        private FSharpMap<string, Tuple<string, FSharpList<Interpreter.terminal>>> storedFunctions = MapModule.Empty<string, Tuple<string, FSharpList<Interpreter.terminal>>>();

        public MainWindow() { InitializeComponent(); }

        private void CalculateOnClick(object sender, Avalonia.Interactivity.RoutedEventArgs e)
        {
            try { (Output.Text, storedVariables, storedFunctions) = Interpreter.main(Input.Text, storedVariables, storedFunctions); }
            catch (Exception error) { Output.Text = $"Error: {error.Message}"; }
        }

        private void PlotOnClick(object sender, Avalonia.Interactivity.RoutedEventArgs e)
        {
            try
            {
                var pointsResult = Interpreter.plot(Input.Text, MinX.Text, MaxX.Text, storedVariables, storedFunctions);
                var points = pointsResult.Item1;
                //check input is integral
                var isIntegral = Input.Text.Contains("integral(");
                PlotPoints(points, isIntegral);
            }
            catch (Exception error)
            {
                Output.Text = $"Error: {error.Message}";
            }
        }

        private void OnPlotMove(object? sender, OxyPlot.Axes.AxisChangedEventArgs e)
        {
            var (minX, maxX) = (PlotView.Model.Axes[0].ActualMinimum, PlotView.Model.Axes[0].ActualMaximum);
            Console.WriteLine($"AxisUpdate - {minX} - {maxX}");
            var points = Interpreter.plot(Input.Text, $"{minX}", $"{maxX}", storedVariables, storedFunctions).Item1;
            var lineSeries = PlotView.Model.Series[0] as LineSeries;
            lineSeries.Points.Clear();
            foreach (var p in points) { lineSeries.Points.Add(new DataPoint(p.Item1, p.Item2)); }

        }

        private void OnZoomKey(object? sender, KeyEventArgs e)
        {
            Console.WriteLine($"Key pressed - {e.Key}");
            var (xAxis, yAxis) = (PlotView.Model.Axes[0], PlotView.Model.Axes[1]);
            var zoomBy = e.Key switch { Key.Add or Key.OemPlus => 0.909, Key.Subtract or Key.OemMinus => 1.1, _ => 0 };
            if (zoomBy != 0)
            {
                xAxis.Zoom(xAxis.ActualMinimum * zoomBy, xAxis.ActualMaximum * zoomBy);
                yAxis.Zoom(yAxis.ActualMinimum * zoomBy, yAxis.ActualMaximum * zoomBy);
                PlotView.InvalidatePlot(false);
            }
        }

        private void PlotPoints(FSharpList<Tuple<double, double>> points, bool isIntegral = false)
        {
            var plotModel = new PlotModel
            {
                Title = isIntegral ? "Numerical Integration" : "Function Plot"
            };

            var lineSeries = new LineSeries
            {
                Title = "f(x)",
                Color = OxyColors.Green
            };

            var areaSeries = new AreaSeries
            {
                Title = "Trapezoidal Area",
                Color = OxyColors.Red,
                //transparent shading
                Fill = OxyColor.FromAColor(102, OxyColors.Red),
                StrokeThickness = 1
            };

            foreach (var p in points)
            {
                lineSeries.Points.Add(new DataPoint(p.Item1, p.Item2));

                //only shade on integration
                if (isIntegral)
                {
                    areaSeries.Points.Add(new DataPoint(p.Item1, p.Item2));
                    areaSeries.Points.Add(new DataPoint(p.Item1, 0));
                }
            }

            if (isIntegral)
            {
                plotModel.Series.Add(areaSeries);
            }

            plotModel.Series.Add(lineSeries);

            plotModel.Axes.Add(new OxyPlot.Axes.LinearAxis
            {
                Position = OxyPlot.Axes.AxisPosition.Bottom,
                Title = "x"
            });

            plotModel.Axes.Add(new OxyPlot.Axes.LinearAxis
            {
                Position = OxyPlot.Axes.AxisPosition.Left,
                Title = "f(x)"
            });

            PlotView.Model = plotModel;
            PlotView.Model.Axes[0].AxisChanged += OnPlotMove;
            KeyDown += OnZoomKey;
        }

    }
}
