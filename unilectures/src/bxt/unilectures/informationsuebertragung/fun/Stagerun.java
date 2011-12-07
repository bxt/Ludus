package bxt.unilectures.informationsuebertragung.fun;

import java.io.UnsupportedEncodingException;
import java.util.List;

public class Stagerun {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		
		Stage<byte[],List<List<Boolean>>> stage = 
				new CombinedStage<byte[],List<Boolean>,List<List<Boolean>>>(
						new CachedStage<byte[], List<Boolean>, Code<Hyte>>(
								new Main())
								, new Blockify());
		
		try {
			stage.tock(stage.tick("Hallo Welt!".getBytes("UTF-8")));
		} catch (UnsupportedEncodingException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

	}

}
