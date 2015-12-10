def perms_for_str_aux (word, new_word, used)
  if new_word.length == word.length then
    puts "#{new_word}"
    return
  end

  for i in 0..word.length-1
    if used[i] then next end

    used[i] = true
    perms_for_str_aux word, new_word + word[i], used
    used[i] = false
  end
end


def perms_for_str (word)
  new_word = ""
  used = word.chars.collect {|ch| false }
  perms_for_str_aux word, new_word, used
end

perms_for_str "abc"
