ʹ��sys_logע�����
1.ǰ�᣺��bin�е���Ӧ�ļ��м���export PUBLIC_LOG_PATH=$ROOT 
 
2._app.erl�м��� ok = dd_util:ensure_app_started(sys_log),
 
3.������ӿڣ�SYS_LOG,ADMIN_LOG,PLAYER_LOG,COM_LOG,ERROR_LOG������ǰ������������Ϊ�б���ʽ����������������file_log_info��file_log_error������

SYS_LOG��ϵͳ������־��ʹ��ʾ������SYS_LOG([...]}
ADMIN_LOG����̨������־��ʹ��ʾ������ADMIN_LOG([...])
PLAYER_LOG����Ҳ�����־��ʹ��ʾ������PLAYER_LOG([...])
COM_LOG�����ò�����־����¼��ˮ�������������÷�������FILE_LOG_INFO,ʹ��ʾ������COM_LOG("...",[...])
ERROR_LOG��������־����¼������Ϣ���������÷�������FILE_LOG_ERROR,ʹ��ʾ������ERROR_LOG("...",[...])

SYS_LOG,ADMIN_LOG,PLAYER_LOG�ӿ�д��ʱ���б��ʽ��
���������ͣ������ߣ��������ߣ��ڵ����ƣ�������������ֵ���������������ǰ��ֵ���������ֵ����ע��Ϣ��    ��10�����ͣ�

����ǰ7������Ϊ�̶���ʽ���������Ӳ�ͬ���ܲ������ݶ������̶���ʽ˵�����£�

�������ͣ�
����ѯquery���޸ģ����£�update�����add��ɾ��delete �ȣ� 

�����ߣ��������ߣ�
ϵͳ��system)����̨����admin�������(uin) ��������� all��

�ڵ����ƣ�
admin,cache,database,gateway,guid,http_proc,log,mail,master ,ranking ,session ,time 

��������
���ܺ�������

���������
�ɹ���success����ʧ�ܣ�fail��

ע��
���в����̶���ʽ������ͬ�����ಿ���Ӳ�ͬ���ܶ���

  