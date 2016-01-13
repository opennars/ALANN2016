using System;
using System.Collections.Generic;
using System.Linq;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Input;
using System.Windows.Media;
using Microsoft.Win32;
using ALANN_GUI.ViewModels;
using System.Diagnostics;
using System.Windows.Threading;
using System.ComponentModel;
using Controller;

using DynamicDataDisplaySample.VoltageViewModel;
using Microsoft.Research.DynamicDataDisplay.DataSources;
using Microsoft.Research.DynamicDataDisplay;

using NLog;

namespace ALANN_GUI
{
    /// <summary>
    /// Interaction logic for MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window, INotifyPropertyChanged
    {
        public IController.IController Controller { get; set; }
        public Controller.ControllerClass EventController { get; set; }
        public ViewModel ViewModel { get; set; }
        private ViewModels.ViewModel viewModel;
        String[] resources = System.Reflection.Assembly.GetExecutingAssembly().GetManifestResourceNames();
        DispatcherTimer splashScreenTimer = new DispatcherTimer();
        //ObservableCollection<string> questions = new ObservableCollection<string>();
        //ObservableCollection<string> results = new ObservableCollection<string>();

        PerformanceCounter cpuCounter = new PerformanceCounter("Processor", "% Processor Time", "_Total");
        PerformanceCounter ramCounter = new PerformanceCounter("Memory", "Available MBytes");

        private int _maxVoltage;
        public int MaxVoltage
        {
            get { return _maxVoltage; }
            set { _maxVoltage = value; this.OnPropertyChanged("MaxVoltage"); }
        }

        private int _minVoltage;
        public int MinVoltage
        {
            get { return _minVoltage; }
            set { _minVoltage = value; this.OnPropertyChanged("MinVoltage"); }
        }

        public VoltagePointCollection voltagePointCollection;
        DispatcherTimer updateCollectionTimer;
        Logger logger = LogManager.GetCurrentClassLogger();

        public MainWindow()
        {
            InitializeComponent();
            viewModel = new ViewModel();
            DataContext = viewModel;

            voltagePointCollection = new VoltagePointCollection();
            updateCollectionTimer = new DispatcherTimer();
            updateCollectionTimer.Interval = TimeSpan.FromMilliseconds(100);
            updateCollectionTimer.Tick += new EventHandler(updateCollectionTimer_Tick);
            updateCollectionTimer.Start();

            InitCharts();
            EventController = new ControllerClass();
            Controller = (IController.IController)(EventController);
            EventController.ParseResult += (m, n) => ListView1.Items.Add(" [" + Controller.Cycle.ToString() + " : Perceived:] " + n.ParsedSentence);
            EventController.ParseError += (m, n) => TextBox1.Text = n.Error;
            EventController.StatusUpdate += (m, n) => StatusBarUpdate(n.Text);
            EventController.TaskCompleted += (m, n) => TaskCompletedFunc(n.Text);
            EventController.InferenceAnswer += (m, n) => DisplayAnswer(n.Answer);
            EventController.NewTasksResult += (m, n) => TaskCount(n.NumNewTasks);
            EventController.ConceptCount += (m, n) => ConceptCount(n.NumNewTasks);
            EventController.Threshold += (m, n) => UpdateThreshold(n.Threshold);
            EventController.InferenceLength += (mbox, n) => UpdateInferenceQueue(n.Length);
            listViewResults.ItemsSource = EventController.CommandResultBuffer;
            //EventController.Answer += (m, s) => UpdateResultsWindow();// resultsWindow.ResultUpdate(s.Answer); // listViewResults.ItemsSource = new ObservableCollection<Types.Task>(s.Answer);
            TaskBar.Minimum = 0;
            TaskBar.Maximum = Parameters.Parameters.NEW_TASKS_PER_CYCLE;
            NumTasksXbeliefs.Text = Parameters.Parameters.INFERENCE_TASKS_PER_CYCLE.ToString() + " x " + Parameters.Parameters.INFERENCE_BELIEFS_PER_CYCLE.ToString();
            EventController.Terms += (m, n) => LoadConceptTree(n.Terms);
            EventController.Metrics += (m, n) => UpdateMetrics(n.Metrics);
            //Application.Current.Exit += Current_Exit;

            cpuCounter.NextValue();
            ramCounter.NextValue();

        }

        private void updateCollectionTimer_Tick(object sender, EventArgs e)
        {
            var oldRect = plotter.Visible;
            oldRect.X = Math.Max(oldRect.X, dateAxis.ConvertToDouble(DateTime.Now.AddMinutes(-0.25)));
            plotter.Visible = oldRect;
        }

        private void InitCharts()
        {
            var ds = new EnumerableDataSource<VoltagePoint>(voltagePointCollection);
            ds.SetXMapping(x => dateAxis.ConvertToDouble(x.Date));
            ds.SetYMapping(y => y.Voltage);

            plotter.LegendVisible = false;
            MaxVoltage = 100;
            MinVoltage = 0;

            var xMin = dateAxis.ConvertToDouble(DateTime.Now);
            var startXMax = dateAxis.ConvertToDouble(DateTime.Now.AddMinutes(0.25));
            var startYMin = -20;
            var startYMax = 6000;
            dateAxis.ShowMayorLabels = false;
            plotter.LegendVisible = false;
            line.DataSource = ds;
            plotter.HorizontalAxis.Remove();
            //plotter.VerticalAxis.Remove();

            plotter.Visible = new Rect { X = xMin, Width = startXMax - xMin, Y = startYMin, Height = startYMax - startYMin };
            plotter.Viewport.AutoFitToView = false;
            //plotter.FitToView();

        }

        private void UpdateMetrics(Types.Metric[] metrics)
        {
            Metric1.Value = cpuCounter.NextValue(); // CPU
            Metric2.Value = 64000 - ramCounter.NextValue(); // Memory
            if (metrics.Length > 0) Metric3.Value = int.Parse(metrics[0].Value);
            if (metrics.Length > 1) Metric4.Value = int.Parse(metrics[1].Value);
            if (metrics.Length > 2) Metric5.Value = int.Parse(metrics[2].Value);
            if (metrics.Length > 3) Metric6.Value = int.Parse(metrics[3].Value);

            voltagePointCollection.Add(new VoltagePoint(float.Parse(metrics[0].Value), DateTime.Now));            
        }

        private void EventController_InferenceResult(object sender, Events.InferenceInfoEventArgs args)
        {
            throw new NotImplementedException();
        }

        private void UpdateThreshold(float threshold)
        {
            Threshold.Text = "AT:" + threshold.ToString("F");
        }

        private void UpdateInferenceQueue(float length)
        {
            NumInferences.Text = "IQ:" + length.ToString();
        }

        private void TaskMeter(int numtasks)
        {
            TaskBar.Value = numtasks;
        }

        private void TaskCount(int numtasks)
        {
            NumTasks.Text = "TD:" + numtasks.ToString();
        }

        private void ConceptCount(int numconcepts)
        {
            NumConcepts.Text = "CC:" + numconcepts.ToString();
        }

        private void StatusBarUpdate(string str)
        {
            StatusText.Text = str;
        }

        private void DisplayInferenceResult(object sender, Events.InferenceInfoEventArgs args)
        {
            if (InferenceLogCheckBox.IsChecked == true && InferenceLogCheckBox.IsEnabled == true)
            {
                ListView2.Items.Add("Time:\t" + args.InferenceCycle);
                ListView2.Items.Add("\tSelected Task:\t " + args.InferenceTask);
                ListView2.Items.Add("\tSelect Belief:\t " + args.InferenceBelief);

                var results = args.InferenceResult as string[];
                foreach (var str in results)
                    ListView2.Items.Add("\t\t\t " + str);
            }
        }

        private void DisplayAnswer(string answer)
        {
            ListView1.Items.Add(" [" + Controller.Cycle.ToString() + " : Answer] " + answer);
            UpdateOutputWindow();
            UpdateResultWindow();
        }

        private void Reset(object sender, RoutedEventArgs e)
        {
            TextBox1.Clear();
            ListView1.Items.Clear();
            ListView2.Items.Clear();
            TreeView1.Items.Clear();
            listViewResults.ItemsSource = null;
            listViewResults.Items.Clear();
            Controller.Reset();
            ConceptTreeTextBlock.Text = "Concept Tree";
            StatusText.Text = "Status: [ready]";
            TaskBar.Value = 0;
            NumConcepts.Text = Parameters.Parameters.INFERENCE_ACTORS.ToString();
            UpdateThreshold(Parameters.Parameters.ACTIVATION_THRESHOLD);
            listViewResults.ItemsSource = EventController.CommandResultBuffer;
        }

        private void UpdateResultWindow()
        {
            var count = listViewResults.Items.Count;

            if (count > 0)
                listViewResults.ScrollIntoView(listViewResults.Items[count - 1]);
        }

        private void UpdateOutputWindow()
        {
            var count = ListView1.Items.Count;

            if (count > 0)
                ListView1.ScrollIntoView(ListView1.Items[count - 1]);
        }

        private void UpdateLogWindow()
        {
            var count = ListView2.Items.Count;

            if (count > 0)
                ListView2.ScrollIntoView(ListView2.Items[count - 1]);
        }

        private void SingleStep(object sender, RoutedEventArgs e)
        {
            logger.Debug("SingleStep");
            // enable inference log with multistep
            InferenceLogCheckBox.IsEnabled = true;

            Controller.Step();
            UpdateLogWindow();
            Controller.UpdateConceptTree();
        }

        private void MultiStep(object sender, RoutedEventArgs e)
        {
            // disable inference log with multistep
            InferenceLogCheckBox.IsEnabled = false;

            MultiStepButton.IsEnabled = false;
            SingleStepButton.IsEnabled = false;
            StopButton.IsEnabled = true;
            ResetButton.IsEnabled = false;
            LoadButton.IsEnabled = true;
            Controller.Execute();
        }

        private void Pause(object sender, RoutedEventArgs e)
        {
            Controller.CancelExecute();

            // re-enable inference log
            InferenceLogCheckBox.IsEnabled = true;
        }

        private void Load(object sender, RoutedEventArgs e)
        {
            // Create OpenFileDialog 
            var dlg = new OpenFileDialog();

            // Set filter for file extension and default file extension 
            dlg.DefaultExt = ".txt";
            dlg.Filter = "Text documents (.txt)|*.txt";

            if (dlg.ShowDialog() == true)
            {
                Controller.LoadNALFile(dlg.FileName);
                UpdateOutputWindow();
                UpdateResultWindow();
                //Controller.UpdateConceptTree();
            }
        }

        private void Save(object sender, RoutedEventArgs e)
        {
            // Create SaveFileDialog 
            var dlg = new SaveFileDialog();

            // Set filter for file extension and default file extension 
            dlg.DefaultExt = ".dot";
            dlg.Filter = "Graph Viz (.dot)|*.dot";

            if (dlg.ShowDialog() == true)
            {
                ;// Controller.ExportGraph(dlg.FileName);
            }
        }

        private void Parse(object sender, KeyEventArgs e)
        {
            if (e.Key == Key.Return && ((e.KeyboardDevice.Modifiers & ModifierKeys.Control) == ModifierKeys.Control))
            {
                Controller.ParseSentence(TextBox1.Text);
                TextBox1.Clear();
                UpdateOutputWindow();
                UpdateResultWindow();
            }
        }

        private void TaskCompletedFunc(string str)
        {
            MultiStepButton.IsEnabled = true;
            SingleStepButton.IsEnabled = true;
            StopButton.IsEnabled = false;
            ResetButton.IsEnabled = true;
            LoadButton.IsEnabled = true;

            StatusText.Text = "Status: ["+ str + "]";
            UpdateLogWindow();
            Controller.UpdateConceptTree();
        }

        private void LoadConceptTree(IEnumerable<Types.ConceptState> terms)
        {
            ConceptTreeTextBlock.Text = "Concept Tree: [" + terms.Count().ToString() + " concepts]";
            var br = new SolidColorBrush(Color.FromRgb(255, 255, 255));
            TreeView1.Items.Clear();

            foreach (var c in terms)
                {
                    // Add concepts
                    var i = new TreeViewItem();
                //i.Header = TermPrinter.TermPrinter.ToString(c.Name);
                i.Header = string.Format("[P{0} {1}]  ", c.Priming.ToString("0.000"), (c.Activation + c.Priming).ToString("0.000")) + TypeFormatters.TypeFormatter.Term(c.Name);
                //i.Header = c;
                i.Tag = c;
                    i.Foreground = br;
                    TreeView1.Items.Add(i);

                //// Add Task header
                var taskHeader = new TreeViewItem();
                taskHeader.Header = "Tasks";
                taskHeader.Tag = c.Tasks;
                taskHeader.Foreground = br;
                i.Items.Add(taskHeader);
                taskHeader.Items.Add("*");

                // Add Belief header
                var beliefHeader = new TreeViewItem();
                beliefHeader.Header = "Beliefs";
                beliefHeader.Tag = c.Beliefs;
                beliefHeader.Foreground = br;
                i.Items.Add(beliefHeader);
                beliefHeader.Items.Add("*");
            }
            //}
        }

        private void item_Expanded(object sendwr, RoutedEventArgs e)
        {
            var br = new SolidColorBrush(Color.FromRgb(255, 255, 255));            
            TreeViewItem item = (TreeViewItem)e.OriginalSource;

            if (item.Header.ToString() == "Tasks" || item.Header.ToString() == "Beliefs")
            {
                item.Items.Clear(); // Remove "*"

                //Add tasks/beliefs
                foreach (var t in (Types.ITaskStore)item.Tag)
                {
                    item_expanded_inner(item, t, br);
                }
            }
        }

        private void item_expanded_inner(TreeViewItem item, Types.Task t, SolidColorBrush br)
        {
            var j = new TreeViewItem();
            j.Header = TypeFormatters.TypeFormatter.ShortTask(t);
            j.Foreground = br;
            item.Items.Add(j);

            // Add Stamp header
            var k = new TreeViewItem();
            k.Header = TypeFormatters.TypeFormatter.StampHeader(t.Stamp);
            k.Foreground = br;
            j.Items.Add(k);

            // Add Trail
            var l = new TreeViewItem();
            l.Header = "Inference Trail: " + TypeFormatters.TypeFormatter.Trail(t.Stamp.Trail);
            l.Foreground = br;
            j.Items.Add(l);

            // Add derivation Trail
            var m = new TreeViewItem();
            m.Header = "Derivation Trail: " + TypeFormatters.TypeFormatter.DerivationTrail(t.Stamp.DerivationTrail);
            m.Foreground = br;
            j.Items.Add(m);

            // Add derivation Rule
            var n = new TreeViewItem();
            n.Header = "Derivation Rule: " + t.Stamp.DerivationRule;
            n.Foreground = br;
            j.Items.Add(n);
        }

        private void item_Collapsed(object sendwr, RoutedEventArgs e)
        {
            TreeViewItem item = (TreeViewItem)e.OriginalSource;

            if (item.Header.ToString() == "Tasks" || item.Header.ToString() == "Beliefs")
            {
                item.Items.Clear();
                item.Items.Add("*");
            }
        }

        private void InferenceLogCheckBox_Checked(object sender, RoutedEventArgs e)
        {
            EventController.InferenceResult += DisplayInferenceResult;
        }

        private void InferenceLogCheckBox_Unchecked(object sender, RoutedEventArgs e)
        {
            EventController.InferenceResult -= DisplayInferenceResult;  
        }

        private void SearchTextBox_TextChanged(object sender, TextChangedEventArgs e)
        {
            for (var i = 0; i < TreeView1.Items.Count; i++)
            {
                var item = TreeView1.Items[i] as TreeViewItem;

                if (item.Header.ToString().Contains(SearchTextBox.Text))
                {
                    item.Focus();
                    break;
                }
            }
            SearchTextBox.Focus();
        }

        public event PropertyChangedEventHandler PropertyChanged;
        protected void OnPropertyChanged(string propertyName)
        {
            if (PropertyChanged != null)
                this.PropertyChanged(this, new System.ComponentModel.PropertyChangedEventArgs(propertyName));
        }
    }
}
