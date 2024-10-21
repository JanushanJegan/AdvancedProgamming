using Avalonia.Controls;
using System;
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
            try { (OutputResult.Text, storedVariables, storedFunctions) = Interpreter.main(InputExpression.Text, storedVariables, storedFunctions); }
            catch (Exception error) { OutputResult.Text = $"Error: {error.Message}"; }
        }

        private void PlotOnClick(object sender, Avalonia.Interactivity.RoutedEventArgs e)
        {
            PlotPoints(Interpreter.plot(InputExpression.Text, MinX.Text, MaxX.Text, storedVariables, storedFunctions).Item1);
        }

        private void PlotPoints(FSharpList<Tuple<double,double>> points)
        {
            var plotModel = new PlotModel ();
            var lineSeries = new LineSeries();
            foreach (var p in points) { lineSeries.Points.Add(new DataPoint(p.Item1, p.Item2)); }
            plotModel.Series.Add(lineSeries);
            plotView1.Model = plotModel;
        }
    }
}
