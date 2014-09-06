require 'curb'
require 'json'
require 'filemagic'

module Libertree
  module RemoteStorage
    # @param [String] the full remote storage account handle, e.g. rekado@my.remotestorage.domain
    # @return [Hash] the remoteStorage definition or nil
    def self.finger(handle)
      host_dirty = handle.split(/@/)[-1]
      return  if host_dirty.nil?
      host = host_dirty.tr('/@:', '')

      url = "http://#{host}/.well-known/host-meta?resource=acct:#{handle}"
      res = Curl::Easy.http_get(url) { |req|
        req.follow_location = true
        req.timeout = 15
      }

      begin
        json = JSON[res.body_str]
        info = json['links'].find {|e| e.keys.include?('rel') && e['rel'] == 'remoteStorage' }
        if valid?(info)
          info
        end
      rescue StandardError => e
        nil
      end
    end

    def self.valid?(info)
      info['rel'] == 'remoteStorage' &&
        info['href'] &&
        ! info['href'].empty? &&
        info['properties'] &&
        info['properties']['auth-method'] == "https://tools.ietf.org/html/draft-ietf-oauth-v2-26#section-4.2" &&
        info['properties']['auth-endpoint'] &&
        ! info['properties']['auth-endpoint'].empty?
    end

    # @param [Hash] the remoteStorage definition
    # @return [String]
    def self.auth_request_url(info, update_token, scope='public/libertree:rw')
      url = URI.parse(info['properties']['auth-endpoint'])
      redirect = "#{$conf['frontend_url_base']}/remotestorage/connection/#{update_token}"
      query_string = URI.encode_www_form({
        "redirect_uri" => redirect,
        "scope" => scope,
        "client_id" => $conf['frontend_url_base']
      })
      if url.query.nil?
        url.query = query_string
      else
        url.query = url.query + "&" + query_string
      end
      url.to_s
    end

    # @return [String] a link on success, nil on failure
    def self.upload(file, storage, path='public/libertree')
      return  unless file.respond_to? :values_at
      tempfile, filename = file.values_at(:tempfile, :filename)

      fm = FileMagic.new(FileMagic::MAGIC_MIME)
      content_type = fm.file(tempfile.path)
      fm.close

      data = IO.read(tempfile.path)

      # append time to file name to avoid overwriting files with the same name
      ext = File.extname(filename)
      basename = File.basename(filename, ext)[0..50] # truncate to 50 chars
      filename = URI.encode("#{basename}-#{Time.now.strftime('%s%4N')}#{ext}")
      remote_url = "#{storage.storage_url}/#{path}/#{filename}"

      Curl::Easy.http_put(remote_url, data) { |req|
        req.timeout = 30 # TODO: how long may the upload take?
        req.headers['Authorization'] = "Bearer #{storage.access_token}"
        req.headers['Content-type'] = content_type

        req.on_success {|easy| return remote_url }
      }

      nil
    end

    def self.delete(path, storage)
      Curl::Easy.http_delete("#{storage.storage_url}/#{path}") do |req|
        req.headers['Authorization'] = "Bearer #{storage.access_token}"
        req.on_success {|easy| return true }
      end
      false
    end

    def self.get(path, storage)
      Curl::Easy.http_get("#{storage.storage_url}/#{path}") do |req|
        req.headers['Authorization'] = "Bearer #{storage.access_token}"
        req.on_success do |res|
          return JSON[res.body_str]
        end
      end
      nil
    end
  end
end
