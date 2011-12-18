package bxt.unilectures.informationsuebertragung.fun.staging;

import java.io.UnsupportedEncodingException;
import java.util.List;

import bxt.unilectures.informationsuebertragung.fun.entropy.Code;
import bxt.unilectures.informationsuebertragung.fun.entropy.Hyte;
import bxt.unilectures.informationsuebertragung.fun.entropy.Entropy;
import bxt.unilectures.informationsuebertragung.fun.redundancy.Blockcode;
import bxt.unilectures.informationsuebertragung.fun.redundancy.Blockify;
import bxt.unilectures.informationsuebertragung.fun.redundancy.Redundancy;

public class Stagerun {

	/**
	 * @param args
	 */
	public static void main(String[] args) {

		Stage<byte[], List<List<Boolean>>> stage = new CombinedStage<byte[], 
			List<Boolean>, List<List<Boolean>>>(
				new CachedStage<byte[], List<Boolean>, Code<Hyte>>(
						new VerbosityStage<byte[], Tuple<Code<Hyte>, 
							List<Boolean>>>(
								new Entropy())),
				new CombinedStage<List<Boolean>, List<List<Boolean>>, 
					List<List<Boolean>>>(
						new VerbosityStage<List<Boolean>, List<List<Boolean>>>(
								new Blockify(7)),
						new CachedStage<List<List<Boolean>>, 
							List<List<Boolean>>, Blockcode>(
								new VerbosityStage<List<List<Boolean>>, 
									Tuple<Blockcode, List<List<Boolean>>>>(
										new Redundancy()))));
		
		try {
			stage.tock(stage.tick("Atlaaaaaaaaaaaz!".getBytes("UTF-8")));
		} catch (UnsupportedEncodingException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

	}

}
