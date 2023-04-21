import java.io.IOException;
import java.util.StringTokenizer;
import java.util.HashMap;
import java.util.TreeMap;
import java.util.Map;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.IntWritable;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.Job;
import org.apache.hadoop.mapreduce.Mapper;
import org.apache.hadoop.mapreduce.Reducer;
import org.apache.hadoop.mapreduce.lib.input.FileInputFormat;
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;

public class CrimeMapReduce {

	public static class FilterMapper
			extends Mapper<Object, Text, Text, IntWritable> {

		private final static IntWritable one = new IntWritable(1);
		private Text crimeType = new Text();

		private boolean takeOnlyOne = true;

		private boolean isYear;
		private Integer yearMonthNumber;
		private String yearMonthString;

		@Override
		public void setup(Context context) throws IOException, InterruptedException {
			Configuration conf = context.getConfiguration();
			isYear = conf.getBoolean("isYear", true);
			yearMonthNumber = conf.getInt("yearMonthNumber", 1);

			if (!isYear) {
				switch (yearMonthNumber) {
					case 1:
						yearMonthString = "January";
					case 2:
						yearMonthString = "February";
					case 3:
						yearMonthString = "March";
					case 4:
						yearMonthString = "April";
					case 5:
						yearMonthString = "May";
					case 6:
						yearMonthString = "June";
					case 7:
						yearMonthString = "July";
					case 8:
						yearMonthString = "August";
					case 9:
						yearMonthString = "September";
					case 10:
						yearMonthString = "October";
					case 11:
						yearMonthString = "November";
					case 12:
						yearMonthString = "December";
					default:
						yearMonthString = "January";
				}
			} else
				yearMonthString = yearMonthNumber.toString();
		}

		public void map(Object key, Text value, Context context) throws IOException, InterruptedException {

			String[] crimeFields = value.toString().split(",");

			if (crimeFields.length == 30) {

				String yearMonth = "";
				if (isYear)
					yearMonth = crimeFields[18].trim();
				else
					yearMonth = crimeFields[19].trim();

				if (yearMonth.equals(yearMonthString)) {
					crimeType.set(crimeFields[11].trim());
					context.write(crimeType, one);
				}
			}
		}
	}

	public static class TopCrimesReducer
			extends Reducer<Text, IntWritable, Text, IntWritable> {
		private IntWritable result = new IntWritable();

		private TreeMap<Integer, Text> countMap = new TreeMap<Integer, Text>();

		private Integer crimeNumber;

		protected void setup(Context context) throws IOException, InterruptedException {
			Configuration conf = context.getConfiguration();
			crimeNumber = conf.getInt("crimeNumber", 3);
		}

		public void reduce(Text key, Iterable<IntWritable> values,
				Context context) throws IOException, InterruptedException {
			int sum = 0;
			for (IntWritable val : values) {
				sum += val.get();
			}
			result.set(sum);
			countMap.put(sum, new Text(key));
		}

		public void cleanup(Context context) throws IOException, InterruptedException {
			int count = 0;

			for (Map.Entry<Integer, Text> entry : countMap.descendingMap().entrySet()) {
				int sum = entry.getKey();
				Text crimeType = entry.getValue();
				context.write(crimeType, new IntWritable(sum));

				count++;
				if (count == crimeNumber) {
					break;
				}
			}
		}
	}

	public static void main(String[] args) throws Exception {
		Configuration conf = new Configuration();
		Job job = Job.getInstance(conf, "Get Most Frequent Crime");

		job.getConfiguration().setBoolean("isYear", args[2].equals("Y"));
		job.getConfiguration().setInt("yearMonthNumber", Integer.parseInt(args[3]));
		job.getConfiguration().setInt("crimeNumber", Integer.parseInt(args[4]));

		job.setJarByClass(CrimeMapReduce.class);
		job.setMapperClass(FilterMapper.class);
		job.setCombinerClass(TopCrimesReducer.class);
		job.setReducerClass(TopCrimesReducer.class);
		job.setOutputKeyClass(Text.class);
		job.setOutputValueClass(IntWritable.class);

		FileInputFormat.addInputPath(job, new Path(args[0]));
		FileOutputFormat.setOutputPath(job, new Path(args[1]));
		System.exit(job.waitForCompletion(true) ? 0 : 1);
	}
}
