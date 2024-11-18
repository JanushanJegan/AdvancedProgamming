using Avalonia.Controls;
using System;
using System.Collections.Generic;
using Avalonia.Input;
using Microsoft.FSharp.Collections;
using OxyPlot;
using OxyPlot.Series;
using InterpreterModule;
namespace GUI.Views
{
    public partial class MainWindow : Window
    {
        private FSharpMap<string, Types.Value> storedVariables = MapModule.Empty<string, Types.Value>();
        private FSharpMap<string, Tuple<string, FSharpList<Types.terminal>>> storedFunctions = MapModule.Empty<string, Tuple<string, FSharpList<Types.terminal>>>();
        private List<string> plottedEquations = new();

        public MainWindow() { InitializeComponent(); }

        private void CalculateOnClick(object sender, Avalonia.Interactivity.RoutedEventArgs e)
        {
            try { (Output.Text, storedVariables, storedFunctions) = Interpreter.main(Input.Text, storedVariables, storedFunctions); }
            catch (Exception error) { Output.Text = $"Error: {error.Message}"; }
        }

        private void PlotOnClick(object sender, Avalonia.Interactivity.RoutedEventArgs e)
        {
            plottedEquations.Clear();
            try {plottedEquations.Add(Input.Text); PlotPoints(Interpreter.plot(Input.Text, MinX.Text, MaxX.Text, storedVariables, storedFunctions).Item1);}
            catch (Exception error) { Output.Text = $"Error: {error.Message}"; }
        }

        private void DifferentiateOnClick(object sender, Avalonia.Interactivity.RoutedEventArgs e)
        {

            try { (Output.Text, storedVariables, storedFunctions) = Interpreter.differentiate(Input.Text, storedVariables, storedFunctions); }
            catch (Exception error) { Output.Text = $"Error: {error.Message}"; }
        }


        private void OnPlotMove(object? sender, OxyPlot.Axes.AxisChangedEventArgs e)
        {
            var (minX, maxX) = (PlotView.Model.Axes[0].ActualMinimum, PlotView.Model.Axes[0].ActualMaximum);
            Console.WriteLine($"AxisUpdate - {minX} - {maxX}");

            for (int i = 0; i < plottedEquations.Count; i++)
            {
                Console.WriteLine($"Plotting {plottedEquations[i]}");
                var lineSeries = PlotView.Model.Series[i] as LineSeries;
                lineSeries.Points.Clear();
                var points = Interpreter.plot(plottedEquations[i], $"{minX}", $"{maxX}", storedVariables, storedFunctions).Item1;
                foreach (var p in points) { lineSeries.Points.Add(new DataPoint(p.Item1, p.Item2)); }
            }
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

        private void PlotPoints(FSharpList<Tuple<double,double>> points)
        {
            var (plotModel, lineSeries) = (new PlotModel(), new LineSeries());
            foreach (var p in points) { lineSeries.Points.Add(new DataPoint(p.Item1, p.Item2)); }
            plotModel.Series.Add(lineSeries);
            PlotView.Model = plotModel;
            PlotView.Model.Axes[0].AxisChanged += OnPlotMove;
            KeyDown += OnZoomKey;
        }

        private void PlotTangent(object sender, Avalonia.Interactivity.RoutedEventArgs e)
        {
            var tangentEqn = Interpreter.getTangentAtPoint(Input.Text, TangX.Text, storedVariables, storedFunctions);
            plottedEquations.Add(tangentEqn);
            var (minX, maxX) = (PlotView.Model.Axes[0].ActualMinimum, PlotView.Model.Axes[0].ActualMaximum);
            var points = Interpreter.plot(tangentEqn, $"{minX}", $"{maxX}", storedVariables, storedFunctions).Item1;
            Console.WriteLine($"Tangent - {tangentEqn}, Points - {points}");
            var lineSeries = new LineSeries();
            foreach (var p in points) { lineSeries.Points.Add(new DataPoint(p.Item1, p.Item2)); }
            PlotView.Model.Series.Add(lineSeries);
            PlotView.InvalidatePlot(false);
        }

    }
}
