using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.ComponentModel;
using System.Globalization;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Data;
using Microsoft.Research.DynamicDataDisplay.Common;

namespace ALANN_GUI.ViewModels
{
    public class TaskToResultConverter : IValueConverter
    {
        public object Convert(object value, Type targetType, object parameter, CultureInfo culture)
        {
            var task = value as Types.Task;
            var questionStr = TypeFormatters.TypeFormatter.Term(task.S.Key.Term);
            var resultStr = TypeFormatters.TypeFormatter.Sentence(task.S.BestAnswer.Value.Sentence);
            return questionStr + " _ " + resultStr;
        }

        public object ConvertBack(object value, Type targetType, object parameter, CultureInfo culture)
        {
            return true;
        }
    }

    public class ViewModel
    {
        ObservableCollection<Models.InferenceData> _ResultsCollection = new ObservableCollection<Models.InferenceData>();
        public ObservableCollection<Models.InferenceData> ResultsCollection { get { return _ResultsCollection; } }

        public ViewModel()
        {
            
        }

       

    }

}

namespace DynamicDataDisplaySample.VoltageViewModel
{
    public class VoltagePointCollection : RingArray<VoltagePoint>
    {
        private const int TOTAL_POINTS = 300;

        public VoltagePointCollection()
            : base(TOTAL_POINTS) // here i set how much values to show 
        {
        }
    }

    public class VoltagePoint
    {
        public DateTime Date { get; set; }

        public double Voltage { get; set; }

        public VoltagePoint(double voltage, DateTime date)
        {
            this.Date = date;
            this.Voltage = voltage;
        }
    }
}