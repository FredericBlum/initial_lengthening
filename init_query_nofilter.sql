SELECT
    phone.cldf_id AS ID,
	phone.cldf_name AS Value,
	1000*phone.duration AS Duration,
    word.cldf_languageReference AS Language,
	language.family AS Family,
    word.speaker_id AS Speaker,
    CASE
        WHEN phone.cldf_id in (select cldf_id FROM utterance_initials) THEN 1 ELSE 0
        END utt_initial, -- whether or not the phone is in utterance initial position
    CASE
        WHEN phone.cldf_id in (select cldf_id FROM word_initials) THEN 1 ELSE 0
        END word_initial, -- whether or not the phone is in word initial position
	sound.cldf_cltsReference AS CLTS,
    -- normalized word length:
	ROUND(((phones_per_word.num_phones - sd_num_phones.avg_num_phones) / sd_num_phones.num_phones), 3) AS z_num_phones,
	-- normalized speech rate of the utterance:
	ROUND(((utt.log_speech_rate - sd_speech_rate.avg_speech_rate) / sd_speech_rate.speech_rate), 3) AS z_speech_rate,
	-- normalized frequency of the word form:
	ROUND(((forms.freq - sd_word_freq.avg_word_freq) / sd_word_freq.word_freq), 3) AS z_word_freq,
	cluster.cluster_status
FROM
    "phones.csv" AS phone,
    "words.csv" AS word, -- word-level metadata joined ON phone.wd_id = word.cldf_id
    ParameterTable AS sound, -- sound-level metadata joined ON phone.cldf_parameterReference = sound.cldf_id
	LanguageTable AS language
LEFT JOIN
	phones_per_word
ON
	phone.wd_id = phones_per_word.wd_id
LEFT JOIN
	forms
ON
	word.cldf_name = forms.form AND
	word.cldf_languageReference = forms.cldf_languageReference
LEFT JOIN
    utterances AS utt  -- utterance-level stats such as speech rate.
ON
    phone.u_ID = utt.u_id
LEFT JOIN -- summary stats on word length per language
    (
        SELECT
            stdev(p.num_phones) AS num_phones,
            AVG(p.num_phones) AS avg_num_phones,
            w.cldf_languageReference
        FROM
            phones_per_word as p
        LEFT JOIN
            'words.csv' AS w
        ON
            p.wd_id = w.cldf_id
        GROUP BY
            w.cldf_languageReference
    ) AS sd_num_phones
ON word.cldf_languageReference = sd_num_phones.cldf_languageReference
LEFT JOIN -- summary stats on speech rate per language
    (
        SELECT
            stdev(log_speech_rate) AS speech_rate,
	        AVG(log_speech_rate) AS avg_speech_rate,
	        cldf_languageReference
        FROM
            utterances
        GROUP BY
            cldf_languageReference
    ) AS sd_speech_rate
ON word.cldf_languageReference = sd_speech_rate.cldf_languageReference
LEFT JOIN -- summary stats on word form frequency per language
    (
        SELECT
            stdev(freq) AS word_freq,
            AVG(freq) AS avg_word_freq,
            cldf_languageReference
        FROM
            forms
        GROUP BY
            cldf_languageReference
    ) AS sd_word_freq
ON 
	word.cldf_languageReference = sd_word_freq.cldf_languageReference
LEFT JOIN
    (
        SELECT
            pp.cldf_id,
            pp.wd_id,
            pp.cldf_name,
            CASE
                WHEN not previous_is_consonant and instr(cldf_cltsreference, 'consonant') > 0 and next_is_consonant and next_wd_id = wd_id THEN 'clusterInitial'
                WHEN ((previous_is_consonant and previous_wd_id = wd_id) or (next_is_consonant and next_wd_id = wd_id)) and instr(cldf_cltsreference, 'consonant') > 0 THEN 'noInitial'
                ELSE 'noCluster'
            END cluster_status
        FROM (
            SELECT
                p.*,
                s.cldf_cltsreference,
                lag(instr(s.cldf_cltsreference, 'consonant') > 0) over () as previous_is_consonant,
                lag(p.wd_id) over () as previous_wd_id,
                lead(instr(s.cldf_cltsreference, 'consonant') > 0) over () as next_is_consonant,
                lead(p.wd_id) over () as next_wd_id
            FROM `phones.csv` as p
            LEFT OUTER JOIN `parametertable` as s on p.cldf_parameterReference = s.cldf_id) as pp
    ) AS cluster
ON
    phone.cldf_id = cluster.cldf_id
WHERE
    phone.wd_id = word.cldf_id AND
    phone.cldf_parameterReference = sound.cldf_id AND
	word.cldf_languageReference = language.cldf_id AND
    -- We only consider non-long, pulmonic consonants ...
    sound.cldf_cltsReference LIKE '%_consonant'
;
