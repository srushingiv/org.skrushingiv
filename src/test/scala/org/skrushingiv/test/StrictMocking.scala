package org.skrushingiv.test

import org.mockito.Mockito.mock
import org.mockito.stubbing.Answer
import org.mockito.invocation.InvocationOnMock

import scala.reflect.ClassTag

/**
 * Mixing in this trait allows you to quickly create mocked objects that throw exceptions
 * when an un-mocked method is invoked instead of returning hard-to-trace nulls.
 */
trait StrictMocking {

  def strictMock[A](implicit ct: ClassTag[A]):A =
    mock(ct.runtimeClass.asInstanceOf[Class[A]], new UnmockedInvocationExceptionAnswer())

}

class UnmockedInvocationExceptionAnswer[A] extends Answer[A] {
  def answer(invocation:InvocationOnMock): A = throw UnmockedInvocationException(invocation)
}

case class UnmockedInvocationException(invocation:InvocationOnMock) extends RuntimeException({
    val args = invocation.getArguments.toSeq.map(_.toString).mkString(", ")
    s"Encountered an invocation of an unmocked method: ${invocation.getMethod.getName}($args)"
  }
)
