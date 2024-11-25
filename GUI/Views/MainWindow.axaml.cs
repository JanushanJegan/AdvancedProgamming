using Avalonia.Controls;
using System;
using Microsoft.FSharp.Collections;

namespace GUI.Views
{
    public partial class MainWindow : Window
    {
        private FSharpMap<string, InterpreterModule.Interpreter.Value> storedVariables = InterpreterModule.Interpreter.getMap();
        public MainWindow() { InitializeComponent(); }
        
        private void CalculateOnClick(object sender, Avalonia.Interactivity.RoutedEventArgs e)
        {
            try { (OutputResult.Text, storedVariables) = InterpreterModule.Interpreter.main(InputExpression.Text, storedVariables); }
            catch (Exception error) { OutputResult.Text = $"Error: {error.Message}"; }
        }
        private void RationalModeButton_Click(object sender, Avalonia.Interactivity.RoutedEventArgs e)
{
    storedVariables = InterpreterModule.Interpreter.setMode("Rational");
    OutputResult.Text = "Rational Mode Enabled";
}

private void ComplexModeButton_Click(object sender, Avalonia.Interactivity.RoutedEventArgs e)
{
    storedVariables = InterpreterModule.Interpreter.setMode("Complex");
    OutputResult.Text = "Complex Mode Enabled";
}

private void ResetModeButton_Click(object sender, Avalonia.Interactivity.RoutedEventArgs e)
{
    storedVariables = InterpreterModule.Interpreter.setMode("Reset");
    OutputResult.Text = "Interpreter Reset";
}



    }

}
