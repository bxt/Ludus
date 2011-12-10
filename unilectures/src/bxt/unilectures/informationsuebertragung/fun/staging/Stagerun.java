package bxt.unilectures.informationsuebertragung.fun.staging;

import java.io.UnsupportedEncodingException;
import java.util.List;

import bxt.unilectures.informationsuebertragung.fun.entropy.Code;
import bxt.unilectures.informationsuebertragung.fun.entropy.Hyte;
import bxt.unilectures.informationsuebertragung.fun.entropy.Entropy;
import bxt.unilectures.informationsuebertragung.fun.redundancy.Blockify;

public class Stagerun {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		
		Stage<byte[],List<List<Boolean>>> stage = 
				new CombinedStage<byte[],List<Boolean>,List<List<Boolean>>>(
						new CachedStage<byte[], List<Boolean>, Code<Hyte>>(
								new Entropy())
								, new Blockify());
		
		try {
			stage.tock(stage.tick("Hallo Welt!".getBytes("UTF-8")));
		} catch (UnsupportedEncodingException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

	}

}
