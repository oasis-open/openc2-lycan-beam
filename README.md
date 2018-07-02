<div>
<h1>README</h1>

<div>
<h2><a id="readme-general">OASIS TC Open Repository: openc2-lycan-beam</a></h2>

<p>This GitHub public repository ( <b><a href="https://github.com/oasis-open/openc2-lycan-beam">https://github.com/oasis-open/openc2-lycan-beam</a></b> ) was created at the request of the <a href="https://www.oasis-open.org/committees/openc2/">OASIS Open Command and Control (OpenC2) TC</a> as an <a href="https://www.oasis-open.org/resources/open-repositories/">OASIS TC Open Repository</a> to support development of open source resources related to Technical Committee work.</p>

<p>While this TC Open Repository remains associated with the sponsor TC, its development priorities, leadership, intellectual property terms, participation rules, and other matters of governance are <a href="https://github.com/oasis-open/openc2-lycan-beam/blob/master/CONTRIBUTING.md#governance-distinct-from-oasis-tc-process">separate and distinct</a> from the OASIS TC Process and related policies.</p>

<p>All contributions made to this TC Open Repository are subject to open source license terms expressed in the <a href="https://www.oasis-open.org/sites/www.oasis-open.org/files/MIT-License.txt">MIT License</a>.  That license was selected as the declared <a href="https://www.oasis-open.org/resources/open-repositories/licenses">"Applicable License"</a> when the TC Open Repository was created.</p>

<p>As documented in <a href="https://github.com/oasis-open/openc2-lycan-beam/blob/master/CONTRIBUTING.md#public-participation-invited">"Public Participation Invited</a>", contributions to this OASIS TC Open Repository are invited from all parties, whether affiliated with OASIS or not.  Participants must have a GitHub account, but no fees or OASIS membership obligations are required.  Participation is expected to be consistent with the <a href="https://www.oasis-open.org/policies-guidelines/open-repositories">OASIS TC Open Repository Guidelines and Procedures</a>, the open source <a href="https://github.com/oasis-open/openc2-lycan-beam/blob/master/LICENSE">LICENSE</a> designated for this particular repository, and the requirement for an <a href="https://www.oasis-open.org/resources/open-repositories/cla/individual-cla">Individual Contributor License Agreement</a> that governs intellectual property.</p>

</div>

<div>
<h2><a id="purposeStatement">Statement of Purpose</a></h2>

<p>Statement of Purpose for this OASIS TC Open Repository (openc2-lycan-beam) as <a href="https://lists.oasis-open.org/archives/openc2/201803/msg00012.html">proposed</a> and <a href="https://www.oasis-open.org/committees/download.php/62755/OpenC2-TC-Minutes-2018-03-21.docx">approved</a> [<a href="https://issues.oasis-open.org/browse/TCADMIN-2866">bis</a>] by the TC:</p>

<p>The purpose of this OASIS TC Open repository is to develop a collection of applications and libraries, coded in languages that run on the BEAM virtual machine (<i>e.g.</i>, <a href="https://en.wikipedia.org/wiki/Erlang_(programming_language)">erlang</a> and <a href="https://elixir-lang.org/">elixir</a>), for the purpose of implementing OpenC2.   Repository assets will demonstrate implementations of OpenC2 that run on beam, including erlang and elixir codebases, to facilitate other other development efforts. [<a href="http://erlang.org/faq/implementations.html#idp32695248">BEAM</a> stands for Bogdan/Bj&ouml;rn's Erlang Abstract Machine -rcc]</p>

<p>This openc2-lycan-beam repository is managed as an open source effort in accordance with the MIT open source license, initially under the leadership and Maintainer role of Duncan Sparrell (sFractal Consulting).</p>

<p>The OASIS OpenC2 Technical Committee was chartered to address matters as they pertain to command and control of cyber defense technologies, and to maintain a library of prototype implementations, sample commands, polyglot implementation and other artifacts as they pertain to the command and control of cyber defense technologies. TC consensus was found to develop and maintain this library as a TC Open Repository.  The OpenC2 TC has already initiated creation of transformation (<i>aka</i> lycan) libraries for python and java.</p>

<!--
<a href="https://en.wikipedia.org/wiki/Erlang_(programming_language)">
<a href="https://elixir-lang.org/">
<a href="http://erlang.org/faq/implementations.html#idp32695248">BEAM</a> stands for Bogdan/Bj&ouml;rn's Erlang Abstract Machine
-->

</div>

<div><h2><a id="purposeClarifications">Additions to Statement of Purpose</a></h2>

trying md without html in middle of all this
 * ist
 * list
add haha and haga

The organization of this repository is a work in progress.
As it grows, it may need reorganization.
For now, it is organized by beam application/library.
 * haha - Https Api Helloworld Actuator
    * ie a very simple do nothing actuator that is conformant to language
 * haga - Https Api GlobalWelcome Actuator
    * ie slighty fancier do nothing actuator that is conformant to language
 * Utils
    * library utilities useful across applications
 * bawnl - Beam AWs Nacl Lycan 
    *  ie a lycan (transformer) that runs on BEAM to interface OpenC2 to the AWS API's for NACL's
 * bawsl - Beam AWs Security group Lycan 
    *  ie a lycan (transformer) that runs on BEAM to interface OpenC2 to the AWS API's for Security Groups
 * baznl - Beam AZure Nacl Lycan 
    *  ie a lycan (transformer) that runs on BEAM to interface OpenC2 to the Azure API's for NACL's
 * bazsl - Beam AZure Security group Lycan 
    *  ie a lycan (transformer) that runs on BEAM to interface OpenC2 to the Azure API's for Security Groups
 *  bdofl - Beam Digital Ocean cloud Firewall Lycan 
    * ie a lycan (transformer) that runs on BEAM to interface OpenC2 to the Digital Ocean Cloud Firewall.
 *  bosfl - Beam OpenStack Fwaas Lycan 
    * ie a lycan (transformer) that runs on BEAM to interface OpenC2 to the Openstack Firewall-as-a-Service (FWaas).

</div>

<div>
<h2><a id="maintainers">Maintainers</a></h2>

<p>TC Open Repository <a href="https://www.oasis-open.org/resources/open-repositories/maintainers-guide">Maintainers</a> are responsible for oversight of this project's community development activities, including evaluation of GitHub <a href="https://github.com/oasis-open/openc2-lycan-beam/blob/master/CONTRIBUTING.md#fork-and-pull-collaboration-model">pull requests</a> and <a href="https://www.oasis-open.org/policies-guidelines/open-repositories#repositoryManagement">preserving</a> open source principles of openness and fairness. Maintainers are recognized and trusted experts who serve to implement community goals and consensus design preferences.</p>

<p>Initially, the associated TC members have designated one or more persons to serve as Maintainer(s); subsequently, participating community members may select additional or substitute Maintainers, per <a href="https://www.oasis-open.org/resources/open-repositories/maintainers-guide#additionalMaintainers">consensus agreements</a>.</p>

<p><b><a id="currentMaintainers">Current Maintainers of this TC Open Repository</a></b></p>

<ul>
<li><a href="mailto:Duncan@sfractal.com">Duncan Sparrell</a>; GitHub ID: <a href="https://github.com/sparrell/">https://github.com/sparrell/</a>; WWW: <a href="https://www.att.com/">sFractal Consulting</a></li>
</ul>

</div>

<div><h2><a id="aboutOpenRepos">About OASIS TC Open Repositories</a></h2>

<p><ul>
<li><a href="https://www.oasis-open.org/resources/open-repositories/">TC Open Repositories: Overview and Resources</a></li>
<li><a href="https://www.oasis-open.org/resources/open-repositories/faq">Frequently Asked Questions</a></li>
<li><a href="https://www.oasis-open.org/resources/open-repositories/licenses">Open Source Licenses</a></li>
<li><a href="https://www.oasis-open.org/resources/open-repositories/cla">Contributor License Agreements (CLAs)</a></li>
<li><a href="https://www.oasis-open.org/resources/open-repositories/maintainers-guide">Maintainers' Guidelines and Agreement</a></li>
</ul></p>

</div>

<div><h2><a id="feedback">Feedback</a></h2>

<p>Questions or comments about this TC Open Repository's activities should be composed as GitHub issues or comments. If use of an issue/comment is not possible or appropriate, questions may be directed by email to the Maintainer(s) <a href="#currentMaintainers">listed above</a>.  Please send general questions about TC Open Repository participation to OASIS Staff at <a href="mailto:repository-admin@oasis-open.org">repository-admin@oasis-open.org</a> and any specific CLA-related questions to <a href="mailto:repository-cla@oasis-open.org">repository-cla@oasis-open.org</a>.</p>

</div></div>
