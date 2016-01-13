using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.ComponentModel;
using System.Drawing;
using System.Globalization;
using System.Runtime.CompilerServices;
using System.Windows;
using System.Windows.Data;

namespace ALANN_GUI.Models
{
    public class InferenceData
    {
        public string Result { get; set; }
    }

    public class Questions : INotifyPropertyChanged
    {
        private string totalQuestions;
        private string unansweredQuestions;

        public string TotalQuestions
        {
            get { return totalQuestions; }
            set
            {
                if (value != totalQuestions)
                {
                    totalQuestions = value;
                    OnPropertyChanged();
                }
            }
        }

        public string UnansweredQuestions
        {
            get { return unansweredQuestions; }
            set
            {
                if (value != unansweredQuestions)
                {
                    unansweredQuestions = value;
                    OnPropertyChanged();
                }
            }
        }

        public event PropertyChangedEventHandler PropertyChanged;

        protected void OnPropertyChanged([CallerMemberName] string propertyName = null)
        {
            if(PropertyChanged != null)
            {
                PropertyChanged(this, new PropertyChangedEventArgs(propertyName));
            }
        }
    }
}
