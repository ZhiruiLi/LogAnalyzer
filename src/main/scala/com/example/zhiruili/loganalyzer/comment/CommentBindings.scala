package com.example.zhiruili.loganalyzer.comment

case class CommentBindings(errorBindings: List[(String, List[(Int, String)])],
                           generalBindings: List[(String, String)])
