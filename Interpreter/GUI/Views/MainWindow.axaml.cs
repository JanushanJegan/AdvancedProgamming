using Avalonia.Controls;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using Avalonia.Input;
using Microsoft.FSharp.Collections;
using OxyPlot;
using OxyPlot.Series;
using InterpreterModule;
using OxyPlot.Legends;

namespace GUI.Views
{
    public partial class MainWindow : Window
    {
        private FSharpMap<string, Types.Value> storedVariables = MapModule.Empty<string, Types.Value>();
        private FSharpMap<string, Tuple<string, FSharpList<Types.terminal>>> storedFunctions = MapModule.Empty<string, Tuple<string, FSharpList<Types.terminal>>>();
        private List<string> plottedEquations = new();
        private Types.Mode mode = Types.Mode.Reset;

        public MainWindow()
        {
            InitializeComponent();
            PlotView.Model = new PlotModel();
        }

        // Event handler for Rational Mode
        private void RationalModeButton_Click(object sender, Avalonia.Interactivity.RoutedEventArgs e)
        {
            Output.Text = "Rational Mode activated!";
            mode = Types.Mode.Rational;
            // Additional logic for enabling Rational Mode
        }

        // Event handler for Complex Mode
        private void ComplexModeButton_Click(object sender, Avalonia.Interactivity.RoutedEventArgs e)
        {
            Output.Text = "Complex Mode activated!";
            mode = Types.Mode.Complex;
            // Additional logic for enabling Complex Mode
        }

        // Event handler for Reset Mode
        private void ResetModeButton_Click(object sender, Avalonia.Interactivity.RoutedEventArgs e)
        {
            Output.Text = "Reset Mode activated!";
            mode = Types.Mode.Reset;
        }

        private void CalculateOnClick(object sender, Avalonia.Interactivity.RoutedEventArgs e)
        {
            try { (Output.Text, storedVariables, storedFunctions) = Interpreter.main(Input.Text, storedVariables, storedFunctions, mode); }
            catch (Exception error) { Output.Text = $"Error: {error.Message}"; }
        }

        private void PlotOnClick(object sender, Avalonia.Interactivity.RoutedEventArgs e)
        {
            if (String.IsNullOrEmpty(Input.Text)) return;
            // plottedEquations.Clear();
            // plottedEquations.Add(Input.Text); PlotPoints(Interpreter.plot(Input.Text, MinX.Text, MaxX.Text, storedVariables, storedFunctions).Item1);
            // try {plottedEquations.Add(Input.Text); PlotPoints(Interpreter.plot(Input.Text, MinX.Text, MaxX.Text, storedVariables, storedFunctions).Item1, Input.Text);}
            // catch (Exception error) { Output.Text = $"Error: {error.Message}"; }

            var (msg, points) = Interpreter.plot(Input.Text, MinX.Text, MaxX.Text, storedVariables, storedFunctions);
            if (points.Length == 0) Output.Text = msg;  // equation is invalid, display error
            else
            {
                var isIntegral = Input.Text.Contains("integral(");
                if (isIntegral) {plottedEquations.Add(Input.Text);}
                else {plottedEquations.Insert(0, Input.Text);}
                Console.WriteLine($"Plotting integral: {isIntegral}");
                PlotPoints(points, Input.Text, isIntegral);

                PlotTangBut.IsEnabled = true;
            }
        }

        private void DifferentiateOnClick(object sender, Avalonia.Interactivity.RoutedEventArgs e)
        {

            try
            {
                (Output.Text, storedVariables, storedFunctions) =
                    Interpreter.differentiate(Input.Text, storedVariables, storedFunctions);
            }
            catch (Exception error)
            {
                Output.Text = $"Error: {error.Message}";
            }
        }

        private void OnPlotMove(object? sender, OxyPlot.Axes.AxisChangedEventArgs e)
        {
            var (minX, maxX) = (PlotView.Model.Axes[0].ActualMinimum, PlotView.Model.Axes[0].ActualMaximum);
            Console.WriteLine($"AxisUpdate - {minX} - {maxX} - {plottedEquations.Count}");
            var seriesId = 0;
            PlotView.Model.Series.ToImmutableList().ForEach(Console.WriteLine);

            Console.WriteLine($"Series: {PlotView.Model.Series.Count}");
            plottedEquations.ForEach(Console.WriteLine);
            Console.WriteLine($"pE: {plottedEquations.Count}");
            for (int i = 0; i < plottedEquations.Count; i++)
            {
                Console.WriteLine($"Plotting {plottedEquations[i]}");
                var lineSeries = PlotView.Model.Series[seriesId] as LineSeries;
                lineSeries!.Points.Clear();
                var points = Interpreter.plot(plottedEquations[i], $"{minX}", $"{maxX}", storedVariables, storedFunctions).Item2;
                foreach (var p in points) { lineSeries.Points.Add(new DataPoint(p.Item1, p.Item2)); }

                if (plottedEquations[i].Contains("integral("))
                {
                    // seriesId++;
                    var areaSeries = PlotView.Model.Series[seriesId] as AreaSeries;
                    areaSeries!.Points.Clear();
                    foreach (var p in points) { areaSeries.Points.Add(new DataPoint(p.Item1, p.Item2)); areaSeries.Points.Add(new DataPoint(p.Item1, 0)); }
                }

                seriesId++;

            }
        }

        private void OnZoomKey(object? sender, KeyEventArgs e)
        {
            if (PlotView.Model.Axes.Count == 0) return;
            Console.WriteLine($"Key pressed - {e.Key}");
            var (xAxis, yAxis) = (PlotView.Model.Axes[0], PlotView.Model.Axes[1]);
            var zoomBy = e.Key switch { Key.Add or Key.OemPlus => 0.909, Key.Subtract or Key.OemMinus => 1.1, _ => 0 };
            if (zoomBy == 0) return;
            xAxis.Zoom(xAxis.ActualMinimum * zoomBy, xAxis.ActualMaximum * zoomBy);
            yAxis.Zoom(yAxis.ActualMinimum * zoomBy, yAxis.ActualMaximum * zoomBy);
            PlotView.InvalidatePlot(false);
        }

        private void PlotPoints(FSharpList<Tuple<double,double>> points, string label, bool isIntegral)
        {
            var (plotModel, lineSeries) = (new PlotModel(), new LineSeries{Title = label});
            var areaSeries = new AreaSeries {
                Title = "Trapezoidal Area",
                Color = OxyColors.Red,
                //transparent shading
                Fill = OxyColor.FromAColor(102, OxyColors.Red),
                StrokeThickness = 1
            };
            plotModel.Legends.Add(new Legend{LegendPosition = LegendPosition.RightTop});
            foreach (var p in points)
            {
                lineSeries.Points.Add(new DataPoint(p.Item1, p.Item2));
                if (isIntegral)
                {
                    areaSeries.Points.Add(new DataPoint(p.Item1, p.Item2));
                    areaSeries.Points.Add(new DataPoint(p.Item1, 0));
                }
            }

            if (PlotView.Model.Axes.Count == 0) // if its the first plot do initial setup
            {

                if (isIntegral) { plotModel.Series.Add(areaSeries); }
                else {plotModel.Series.Add(lineSeries);}
                PlotView.Model = plotModel;
                PlotView.Model.Axes[0].MajorGridlineStyle = LineStyle.Solid;
                // PlotView.Model.Axes[0].MinorGridlineStyle = LineStyle.Dot;
                PlotView.Model.Axes[1].MajorGridlineStyle = LineStyle.Solid;
                // PlotView.Model.Axes[1].MinorGridlineStyle = LineStyle.Dot;
                PlotView.Model.Axes[0].AxisChanged += OnPlotMove;
                KeyDown += OnZoomKey;
            }
            else
            {

                if (isIntegral) { PlotView.Model.Series.Add(areaSeries); }
                else {PlotView.Model.Series.Insert(0, lineSeries);}
                PlotView.InvalidatePlot(true);
            }



        }

        private void ResetPlot(object sender, Avalonia.Interactivity.RoutedEventArgs e)
        {
            plottedEquations.Clear();
            PlotView.Model = new PlotModel();
            PlotTangBut.IsEnabled = false;
        }

        private void PlotTangent(object sender, Avalonia.Interactivity.RoutedEventArgs e)
        {
            // if (plottedEquations.Count == 0) { Output.Text = "No Plot"; return; }
            // if (TangX.Text.Length == 0) { Output.Text = "No point to plot tangent at"; return; }
            var (tangentEqn, msg) = Interpreter.getTangentAtPoint(plottedEquations.First(), TangX.Text, storedVariables, storedFunctions);
            if (String.IsNullOrEmpty(tangentEqn)) {Output.Text = msg; return;}
            plottedEquations.Add(tangentEqn);
            var (minX, maxX) = (PlotView.Model.Axes[0].ActualMinimum, PlotView.Model.Axes[0].ActualMaximum);
            var points = Interpreter.plot(tangentEqn, $"{minX}", $"{maxX}", storedVariables, storedFunctions).Item2;
            Console.WriteLine($"Tangent - {tangentEqn}, Points - {points}");
            var lineSeries = new LineSeries{Title = tangentEqn};
            foreach (var p in points) { lineSeries.Points.Add(new DataPoint(p.Item1, p.Item2)); }
            PlotView.Model.Series.Add(lineSeries);
            PlotView.InvalidatePlot(false);
        }

    }
}
