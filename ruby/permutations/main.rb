def perms_for_str_aux (word, new_word, used, level)
  if level == word.length then
    puts "#{new_word}"
    return
  end

  for i in 0..word.length-1
    if used[i] then next end

    new_word += word[i]
    used[i] = true
    perms_for_str_aux word, new_word, used, level+1
    used[i] = false
    new_word = new_word.slice(0, new_word.length-1)
  end
end


def perms_for_str (word)
  new_word = ""
  used = word.chars.collect {|ch| false }
  perms_for_str_aux word, new_word, used, 0
end

perms_for_str "abc"
