package com.keydatasys.conversion.qti.util

import org.specs2.mutable.Specification

import scala.xml.XML

class EntityEscaperSpec extends Specification with EntityEscaper {

  "encodeSafeEntities" should {

    val passage = <div xmlns="http://www.imsglobal.org/xsd/apip/apipv1p0/qtiitem/imsqti_v2p1">
      <!--<p class="passage-intro">Today you will read the folktale &#x201C;The Fox and the Horse.&#x201D; As you read, pay close attention to characters and events as you answer the questions to prepare to write a narrative story.</p>-->
      <p class="passage" id="psg_p001">Read the folktale &#x201C;The Fox and the Horse.&#x201D; Then answer the questions.</p>
      <p class="passage-title" id="psg_p002">The Fox and the Horse</p>
      <p id="psg_p003"><span class="badge badge-inverse" id="psg_spn001">1</span>	A peasant once had a faithful horse, but it had grown old and could no longer do its work. Its master grudged it food, and said: &#x201C;I can&#x2019;t use you any more, but I still feel kindly towards you, and if you show yourself strong enough to bring me a lion I will keep you to the end of your days. But away with you now, out of my stable&#x201D;; and he drove it out into the open country.<br/>
      </p>
      <p id="psg_p004"><span class="badge badge-inverse" id="psg_spn002">2</span>	The poor horse was very sad, and went into the forest to get a little shelter from the wind and weather. There he met a fox, who said: &#x201C;Why do you hang your head, and wander about in this solitary fashion?&#x201D;<br/>
      </p>
      <p id="psg_p005"><span class="badge badge-inverse" id="psg_spn003">3</span>	&#x201C;Alas!&#x201D; answered the horse, &#x201C;avarice and honesty cannot live together. My master has forgotten all the service I have done him for these many years, and because I can no longer plough he will no longer feed me, and he has driven me away.&#x201D;<br/>
      </p>
      <p id="psg_p006"><span class="badge badge-inverse" id="psg_spn004">4</span>	&#x201C;Without any consideration?&#x201D; asked the fox.<br/>
      </p>
      <p id="psg_p007"><span class="badge badge-inverse" id="psg_spn005">5</span>	&#x201C;Only the poor consolation of telling me that if I was strong enough to bring him a lion he would keep me, but he knows well enough that the task is beyond me.&#x201D;<br/>
      </p>
      <p id="psg_p008"><span class="badge badge-inverse" id="psg_spn006">6</span>	The fox said: &#x201C;But I will help you. Just you lie down here, and stretch your legs out as if you were dead.&#x201D; The horse did as he was told, and the fox went to the lion&#x2019;s den, not far off, and said: &#x201C;There is a dead horse out there. Come along with me, and you will have a rare meal.&#x201D; The lion went with him, and when they got up to the horse, the fox said: &#x201C;You can&#x2019;t eat it in comfort here. I&#x2019;ll tell you what. I will tie it to you, and you can drag it away to your den, and enjoy it at your leisure.&#x201D;<br/>
      </p>
      <p id="psg_p009"><span class="badge badge-inverse" id="psg_spn007">7</span>	The plan pleased the lion, and he stood quite still, close to the horse, so that the fox should fasten them together. But the fox tied the lion&#x2019;s legs together with the horse&#x2019;s tail, and twisted and knotted it so that it would be quite impossible for it to come undone.<br/>
      </p>
      <p id="psg_p010"><span class="badge badge-inverse" id="psg_spn008">8</span>	When he had finished his work he patted the horse on the shoulder, and said: &#x201C;Pull, old grey! Pull!&#x201D;<br/>
      </p>
      <p id="psg_p011"><span class="badge badge-inverse" id="psg_spn009">9</span>	Then the horse sprang up, and dragged the lion away behind him. The lion in his rage roared, so that all the birds in the forest were terrified, and flew away. But the horse let him roar, and never stopped till he stood before his master&#x2019;s door.<br/>
      </p>
      <p id="psg_p012"><span class="badge badge-inverse" id="psg_spn010">10</span>	When the master saw him he was delighted, and said to him: &#x201C;You shall stay with me, and have a good time as long as you live.&#x201D;<br/>
      </p>
      <p id="psg_p013"><span class="badge badge-inverse" id="psg_spn011">11</span>	And he fed him well till he died.</p>
      <br/>
      <p id="psg_p014">&#x201C;The Fox and the Horse&#x201D;&#x2014;Public Domain</p>
    </div>

    "encode &quot;" in {
      println(unescapeEntities(escapeEntities(passage.toString)))
      true === true
    }

  }

}