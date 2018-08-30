# TODO: Write documentation for `Permutations`

def perms_for_str_aux (word, new_word, used)
  if new_word.size == word.size
    puts "#{new_word}"
    return
  end

  (0..word.size-1).each do |i|
    if used[i] == true
      next
    end
    used[i] = true
    perms_for_str_aux word, new_word + word[i], used
    used[i] = false
  end
end


def perms_for_str (word)
  new_word = ""
  used = Array(Bool).new(word.size) {|i| false}
  perms_for_str_aux word, new_word, used
end

perms_for_str "abc"
