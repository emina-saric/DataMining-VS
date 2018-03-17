using System.Linq;

namespace DataGeneration
{
    public class Generator
    {
        public double[] GetVentSizes()
        {
            MathNet.Numerics.Distributions.ContinuousUniform uniformDistribution = new MathNet.Numerics.Distributions.ContinuousUniform(0.8, 1.5);
            return uniformDistribution.Samples().ToArray();
        }
    }
}
