function temp () {
    cpu=$(</sys/class/thermal/thermal_zone0/temp)
    echo "$(date) @ $(hostname)"
    echo "-------------------------------------------"
    echo "GPU => $(cut -d'=' -f2 <<< $(/opt/vc/bin/vcgencmd measure_temp))"
    echo "CPU => $((cpu/1000))'C"
}
