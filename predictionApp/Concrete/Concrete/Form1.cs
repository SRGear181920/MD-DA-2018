using System;
using System.Windows.Forms;
using System.IO;
using RDotNet;

namespace Concrete
{
    public partial class Form1 : Form
    {
        REngine engine;
        public Form1()
        {
            InitializeComponent();

        }

        private void Form1_Load(object sender, EventArgs e)
        {

            REngine.SetEnvironmentVariables();
            StartupParameter rinit = new StartupParameter();
            rinit.Quiet = true;
            rinit.RHome = @"C:\Program Files\R\R-3.4.0";
            rinit.Interactive = true;
            engine = REngine.GetInstance(null, true, rinit);
            engine.Initialize();
            var rScript = File.ReadAllText(@"..\..\Data\Concretes.r");
            engine.Evaluate(rScript);
        }

        private void Clear()
        {
            textBox1.Text = "0,00";
            textBox2.Text = "0,00";
            textBox3.Text = "0,00";
            textBox4.Text = "0,00";
            textBox5.Text = "0,00";
            textBox6.Text = "0,00";
            textBox7.Text = "0,00";
            textBox8.Text = "0,00";
            labelAnswer.Text = "";
        }

        private void button1_Click(object sender, EventArgs e)
        {
            try
            {
                var dataIn = string.Format("data.frame(Cement={0}, Blast.Furnace.Slag={1}, Fly.Ash={2}, Water={3},",
                    double.Parse(textBox1.Text).ToString(),
                    double.Parse(textBox2.Text).ToString(),
                    double.Parse(textBox3.Text).ToString(),
                    double.Parse(textBox4.Text).ToString());
                dataIn = string.Format(dataIn + "Superplasticizer={0}, Coarse.Aggregate={1}, Fine.Aggregate={2}, Age={3})",
                    double.Parse(textBox5.Text).ToString(),
                    double.Parse(textBox6.Text).ToString(),
                    double.Parse(textBox7.Text).ToString(),
                    double.Parse(textBox8.Text).ToString());
                var script = string.Format("predict(fit.rf, {0})", dataIn);
                var answer = engine.Evaluate(script).AsCharacter().ToArray()[0];
                labelAnswer.Text = answer.ToString();
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.ToString(), "Ошибка!", MessageBoxButtons.OK, MessageBoxIcon.Information);
                Clear();
            }
        }

    }
}
